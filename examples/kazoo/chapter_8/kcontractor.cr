require "./visitor"
require "./llvm_binding_extensions"
require "./kast"

module Kazoo
  ZERO = LLVM.double(0.0)

  class Contractor
    include CLTK::Visitor

    getter :main_module
    @fpm : LLVM::FunctionPassManager

    def initialize(@builder = LLVM::Builder.new)
      LLVM.init_x86
      @main_module = LLVM::Module.new("Kazoo JIT");
      @st = {} of String => LLVM::Value
      @env = @builder
      # Execution Engine
      @engine = LLVM::JITCompiler.new(@main_module)

      @fpm = @main_module.new_function_pass_manager
      # Add passes to the Function Pass Manager.
      LibLLVM.add_instruction_combining_pass(@fpm)
      LibLLVM.add_reassociate_pass(@fpm)
      LibLLVM.add_gvn_pass(@fpm)
      LibLLVM.add_cfg_simplification_pass(@fpm)
      LibLLVM.add_promote_memory_to_register_pass(@fpm)
    end

    def execute(func, args = [] of LLVM::GenericValue)
      @engine.run_function(func)
    end

    def optimize(func : LLVM::Function)
      @fpm.run { |runner | runner.run(func) }
      func
    end

    def with_builder(builder)
      old_env = @env
      @env = builder
      result = with builder yield
      @env = old_env
      result
    end

    def add(ast)
      case ast
      when Function, Prototype then visit ast
      when Expression          then visit Function.new(Kazoo::Prototype.new("", [] of CLTK::ASTNode), ast)
      else raise "Attempting to add an unhandled node type to the JIT."
      end as LLVM::Function
    end

    on Assign do |node|
      right = visit node.right
      name = node.name.not_nil!
      loc =
    	if @st.has_key?(name)
    	  @st[name]
    	else
    	  @st[name] = alloca LLVM::Double, name
    	end
      store(right, loc)
      right
    end

    on Variable do |node|
      name = node.name.not_nil!
      if @st[name]?
	   load @st[name], name
      else
	raise "Unitialized variable \"#{name}\"."
      end
    end

    on Call do |node|
      callee = @main_module.functions[node.name.not_nil!]
      if !callee
	raise "Unknown function referenced."
      end
      args = (node.args as Array)
      if callee.params.size != args.size
	raise "Function #{node.name} expected #{callee.params.size} argument(s) but was called with #{args.size}."
      end

      args = args.map { |arg| (visit arg) as LLVM::Value }
      call callee, args, "calltmp"
    end

    on Prototype do |node|
      name = node.name.not_nil!
      func = begin
               # get function if it"s already defined
               @main_module.functions[name].tap do |func|
	         if LibLLVM.count_basic_blocks(func) != 0
	           raise "Redefinition of function #{name}."
	         elsif func.params.size != (node.arg_names as Array).size
	           raise "Redefinition of function #{name} with different number of arguments."
	         end
               end
             rescue
               # add function, if not
	       @main_module.functions.add(name, Array.new((node.arg_names as Array).size, LLVM::Double), LLVM::Double)
             end
      # Name each of the function paramaters.
      func.tap do
	node.arg_names.each_with_index do |name, i|
	  func.params[i].name = name
	end
      end
    end

    on Function do |node|
      # Reset the symbol table.
      # @st.clear

      # Translate the function"s prototype.
      func = visit node.proto as Prototype
      func.params.to_a.each do |param|
        name = param.name.not_nil!
	@st[name] = alloca LLVM::Double, name
	store param, @st[param.name]
      end
      # Create a new basic block to insert into, allocate space for
      # the arguments, store their values, translate the expression,
      # and set its value as the return value.
      func.basic_blocks.append("entry") do |builder|
        with_builder(builder) do
          body = node.body
          case body
          when ExpressionList then
            expressions = body.expressions as Array
            expressions.each_with_index do |expression, index|
              if index < (expressions.size - 1)
                visit expression
              else
                ret visit(expression)
              end
            end
          else
            ret visit(body)
          end
        end
      end


      # Verify the function and return it.
      func.tap do |func|
        LibLLVM.verify_function(func, LLVM::VerifierFailureAction::ReturnStatusAction )
      end # .tap &.dump
      func
    end

    on For do |node|
      ph_bb = insert_block
      func = LLVM::Function.new LibLLVM.get_basic_block_parent(ph_bb)

      loop_cond_bb = func.basic_blocks.append("loop_cond")

      var_name = node.var.not_nil!
      loc = alloca LLVM::Double, var_name
      store (visit node.init), loc

      old_var = @st[var_name]? ? @st[var_name] : loc
      @st[var_name] = loc

      br loop_cond_bb
      position_at_end(loop_cond_bb)
      branch_body = visit(node.cond)
      end_cond = fcmp LLVM::RealPredicate::ONE, branch_body , ZERO, "loopcond"

      loop_bb1 = nil
      loop_bb0 = func.basic_blocks.append("loop") do |builder|
        with_builder(builder) do
         visit node.body
         loop_bb1 = builder.insert_block
         step_val = visit node.step
         var	  = load loc, var_name
         next_var = fadd var, step_val, "nextvar"
         store next_var, loc

         br loop_cond_bb

        end
      end


      # Add the conditional branch to the loop_cond_bb.
      after_bb = func.basic_blocks.append("afterloop") do |builder|
        with_builder(builder) do
          position_at_end(loop_cond_bb)
        end
      end

      cond end_cond, loop_bb0, after_bb

      position_at_end(after_bb)

      @st[var_name] = old_var

      ZERO
    end

    on If do |node|
      # IF
      cond_val = fcmp LLVM::RealPredicate::UGT, (visit node.cond), ZERO, "ifcond"
      table = LLVM::PhiTable.new

      start_bb = insert_block
      func      = LLVM::Function.new LibLLVM.get_basic_block_parent(start_bb)

      ## THEN
      new_then_bb = nil
      then_bb               = func.basic_blocks.append("then") do |builder|
        then_val, new_then_bb = with_builder(builder) do
          { visit(node.elseExp), builder.insert_block }
        end
        table.add(new_then_bb, then_val)
      end

      ## ELSE
      new_else_bb = nil
      else_bb               = func.basic_blocks.append("else") do |builder|
        else_val, new_else_bb = with_builder(builder) do
          { visit(node.thenExp), builder.insert_block }
        end
        table.add(new_else_bb, else_val)
      end

      merge_bb = func.basic_blocks.append("merge")

      position_at_end(merge_bb)
      phi_inst = phi LLVM::Double, table, "iftmp"

      position_at_end(start_bb)
      cond cond_val, then_bb, else_bb

      position_at_end(new_then_bb.not_nil!)
      br merge_bb

      position_at_end(new_else_bb.not_nil!)
      br merge_bb

      phi_inst.tap { position_at_end merge_bb }
    end

    on Binary do |node|
      left  = visit node.left
      right = visit node.right

      case node
      when Add then fadd(left, right, "addtmp")
      when Sub then fsub(left, right, "subtmp")
      when Mul then fmul(left, right, "multmp")
      when Div then fdiv(left, right, "divtmp")
      when LT  then ui2fp(fcmp(LLVM::RealPredicate::ULT, left, right, "cmptmp"), LLVM::Double, "lttmp")
      when GT  then ui2fp(fcmp(LLVM::RealPredicate::UGT, left, right, "cmptmp"), LLVM::Double, "gttmp")
      when Eql then ui2fp(fcmp(LLVM::RealPredicate::UEQ, left, right, "cmptmp"), LLVM::Double, "eqtmp")
      when Or
	left  = fcmp LLVM::RealPredicate::UNE,  left, ZERO, "lefttmp"
	right = fcmp LLVM::RealPredicate::UNE, right, ZERO, "righttmp"

	ui2fp (@env.or left, right, "ortmp"), LLVM::Double, "orltmp"
      when And
	left  = fcmp LLVM::RealPredicate::UNE,  left, ZERO, "lefttmp"
	right = fcmp LLVM::RealPredicate::UNE, right, ZERO, "rightmp"

	ui2fp (@env.and left, right, "andtmp"), LLVM::Double, "andtmp"
      else right
      end
    end

    on ANumber do |node|
      LLVM.double(node.value.not_nil!)
    end
  end
end
