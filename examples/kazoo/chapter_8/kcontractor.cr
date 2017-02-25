require "./visitor"
require "./llvm_binding_extensions"
require "./kast"

module Kazoo

  class Contractor
    include CLTK::Visitor(LLVM::Builder)

    getter :main_module
    @fpm : LLVM::FunctionPassManager
    @ctx : LLVM::Context
    @main_module : LLVM::Module
    @builder : LLVM::Builder

    def zero
      @ctx.float.const_double(0.0)
    end

    def initialize()
      LLVM.init_x86
      @ctx = LLVM::Context.new
      @main_module = @ctx.new_module("Kazoo JIT")
      @builder = @ctx.new_builder

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
       @engine.run_function(func, @ctx)
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
      when Expression          then visit Function.new(
                                            proto: Kazoo::Prototype.new(name: "", arg_names: [] of String),
                                            body: ast)
      else raise "Attempting to add an unhandled node type to the JIT."
      end.as(LLVM::Function)
    end

    on Assign do |node|
      right = visit node.right
      loc =
    	if @st.has_key?(node.name)
    	  @st[node.name]
    	else
    	  @st[node.name] = alloca @ctx.float, node.name
    	end
      store(right, loc)
      right
    end

    on Variable do |node|
      if @st[node.name]?
	   load @st[node.name], node.name
      else
	raise "Unitialized variable \"#{node.name}\"."
      end
    end

    on Call do |node|
      callee = @main_module.functions[node.name]
      if !callee
	raise "Unknown function referenced."
      end
      if callee.params.size != node.args.size
	raise "Function #{node.name} expected #{callee.params.size} argument(s) but was called with #{node.args.size}."
      end


      call callee,
           node.args.map { |arg| (visit arg).as(LLVM::Value) },
           "calltmp"
    end

    on Prototype do |node|
      func = begin
               # get function if it"s already defined
               @main_module.functions[node.name].tap do |func|
	         if LibLLVM.count_basic_blocks(func) != 0
	           raise "Redefinition of function #{node.name}."
	         elsif func.params.size != node.arg_names.size
	           raise "Redefinition of function #{node.name} with different number of arguments."
	         end
               end
             rescue
               # add function, if not
	       @main_module.functions.add(node.name, Array.new(node.arg_names.size, @ctx.float), @ctx.float)
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
      func = visit node.proto.as(Prototype)
      func.params.to_a.each do |param|
	@st[param.name] = alloca @ctx.float, param.name
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
            expressions = body.expressions
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

      loc = alloca @ctx.float, node.var
      store (visit node.init), loc

      old_var = @st[node.var]? ? @st[node.var] : loc
      @st[node.var] = loc

      br loop_cond_bb
      position_at_end(loop_cond_bb)
      branch_body = visit(node.cond)
      end_cond = fcmp LLVM::RealPredicate::ONE, branch_body , zero, "loopcond"

      loop_bb1 = nil
      loop_bb0 = func.basic_blocks.append("loop") do |builder|
        with_builder(builder) do
         visit node.body
         loop_bb1 = builder.insert_block
         step_val = visit node.step
         var	  = load loc, node.var
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

      @st[node.var] = old_var

      zero
    end

    on If do |node|
      # IF
      cond_val = fcmp LLVM::RealPredicate::UGT, (visit node.cond), zero, "ifcond"
      table = LLVM::PhiTable.new

      start_bb = insert_block
      func      = LLVM::Function.new LibLLVM.get_basic_block_parent(start_bb)

      ## THEN
      new_then_bb = nil
      then_bb               = func.basic_blocks.append("then") do |builder|
        then_val, new_then_bb = with_builder(builder) do
          { visit(node.elseExp), builder.insert_block }
        end
        table.add(new_then_bb, then_val.as(LLVM::Value))
      end

      ## ELSE
      new_else_bb = nil
      else_bb               = func.basic_blocks.append("else") do |builder|
        else_val, new_else_bb = with_builder(builder) do
          { visit(node.thenExp), builder.insert_block }
        end
        table.add(new_else_bb, else_val.as(LLVM::Value))
      end

      merge_bb = func.basic_blocks.append("merge")

      position_at_end(merge_bb)
      phi_inst = phi @ctx.float, table, "iftmp"

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
      when LT  then ui2fp(fcmp(LLVM::RealPredicate::ULT, left, right, "cmptmp"), @ctx.float, "lttmp")
      when GT  then ui2fp(fcmp(LLVM::RealPredicate::UGT, left, right, "cmptmp"), @ctx.float, "gttmp")
      when Eql then ui2fp(fcmp(LLVM::RealPredicate::UEQ, left, right, "cmptmp"), @ctx.float, "eqtmp")
      when Or
	left  = fcmp LLVM::RealPredicate::UNE,  left, zero, "lefttmp"
	right = fcmp LLVM::RealPredicate::UNE, right, zero, "righttmp"

	ui2fp (@env.or left, right, "ortmp"), @ctx.float, "orltmp"
      when And
	left  = fcmp LLVM::RealPredicate::UNE,  left, zero, "lefttmp"
	right = fcmp LLVM::RealPredicate::UNE, right, zero, "rightmp"

	ui2fp (@env.and left, right, "andtmp"), @ctx.float, "andtmp"
      else right
      end
    end

    on ANumber do |node|
      @ctx.float.const_double(node.value)
    end
  end
end
