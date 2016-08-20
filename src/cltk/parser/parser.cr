require "./state"

module CLTK
  class Parser
    class Parser
      @symbols: Array(String) | Nil
      @lh_sides: Hash(Int32, String)
      @states: Array(State)
      @procs: Hash(Int32, {ProdProc, Int32})
      @token_hooks: Hash(String, Array(Proc(Environment, Nil)))
      @env : (Environment.class) | Nil

      def initialize(@symbols, @lh_sides, @states, @procs, @token_hooks, @env); end

      private def build_parse_opts(opts : NamedTuple?)
        {
	  accept:     :first,
	  env:        (@env || Environment).new,
	  parse_tree: nil,
	  verbose:    nil
        }.merge(opts)
      end

      def parse(tokens, popts : NamedTuple?)
        if @symbols.nil?
	  raise "\n\n!!! The Parser doesn't have any Symbols defined, did you forget to call the 'finalize' class method ?\n\n"
        end

        # Get the full options hash.
        opts = build_parse_opts(popts)
        v = STDOUT

        if opts[:verbose]
	  v.puts("Input tokens:")
	  v.puts(tokens.map { |t| t.type }.inspect)
	  v.puts
        end

        # Stack IDs to keep track of them during parsing.
        stack_id = 0

        # Error mode indicators.
        error_mode      = false
        reduction_guard = false

        # Our various list of stacks.
        accepted   = [] of ParseStack
        moving_on  = [] of ParseStack
        processing = [ParseStack.new(stack_id += 1)]
        # Iterate over the tokens.  We don't procede to the
        # next token until every stack is done with the
        # current one.
        tokens.each_with_index do |token, index|
	  # Check to make sure this token was seen in the
	  # grammar definition.
          unless @symbols.not_nil!.includes?(token.type.to_s)
	    raise BadToken.new(token)
          end
          st = if token.value
                 "(" + (token.value.to_s as String) + ")"
               else
                 ""
               end
          if opts[:verbose]
	    v.puts("Current token: #{token.type}#{st}")
          end
	  # Iterate over the stacks until each one is done.

	  while (processing.size > 0)
            stack = processing.shift
	    # Execute any token hooks in this stack's environment.
	    @token_hooks.not_nil![token.type.to_s].each { |hook| hook.call(opts[:env])}

	    # Get the available actions for this stack.
	    actions = @states.not_nil![stack.state as Int32].on?(token.type.to_s)
	    if actions.empty?
	      # If we are already in error mode and there
	      # are no actions we skip this token.

	      if error_mode
                st = if token.value
                       "(" + (token.value.to_s as String) + ")"
                     end
	        if opts[:verbose]
	          v.puts("Discarding token: #{token.type}#{st}") if v
                end
	        # Add the current token to the array
	        # that corresponds to the output value
	        # for the ERROR token.
                (stack.output_stack.last as Array) << token
	        moving_on << stack
	        next
	      end

	      # We would be dropping the last stack so we
	      # are going to go into error mode.
	      if accepted.empty? && moving_on.empty? && processing.empty?

	        if opts[:verbose]
	          v.puts
	          v.puts("Current stack:")
	          v.puts("\tID: #{stack.id}")
	          v.puts("\tState stack:\t#{stack.state_stack.inspect}")
	          v.puts("\tOutput Stack:\t#{stack.output_stack.inspect}")
	          v.puts
	        end

	        # Try and find a valid error state.
	        while stack.state_stack.size > 0
	          if (actions = @states.not_nil![stack.state].on?(:ERROR.to_s)).empty?
	            # This state doesn't have an
	            # error production. Moving on.
                    stack.pop
	          else
	            # Enter the found error state.
	            stack.push(actions.first.id, [token], :ERROR.to_s, token.position)
	            break
	          end
	        end
	        if stack.state_stack.size > 0
	          # We found a valid error state.
	          error_mode = reduction_guard = true
	          (opts[:env] as Environment).he = true
	          moving_on << stack

	          if opts[:verbose]
	            v.puts("Invalid input encountered.  Entering error handling mode.")
	            v.puts("Discarding token: #{token.type}(#{token.value})")
		  end
	        else
		  # No valid error states could be
		  # found.  Time to print a message
		  # and leave.
	          if opts[:verbose]
                    v.puts("No more actions for stack #{stack.id}.  Dropping stack.") if v
                  end
	        end
	      else
                if opts[:verbose]
	          v.puts("No more actions for stack #{stack.id}.  Dropping stack.") if v
                end
	      end

	      next
	    end

	    # Make (stack, action) pairs, duplicating the
	    # stack as necessary.
	    pairs = ([{stack, actions.pop}] + actions.map {|action| {stack.branch(stack_id += 1), action} })
	    pairs.each do |pair|
              stack = pair[0] as ParseStack
              action = pair[1] as Action

	      if opts[:verbose]
	        v.puts
	        v.puts("Current stack:")
	        v.puts("\tID: #{stack.id}")
	        v.puts("\tState stack:\t#{stack.state_stack.inspect}")
	        v.puts("\tOutput Stack:\t#{stack.output_stack.inspect}")
	        v.puts
	        v.puts("Action taken: #{action.to_s}")
	      end

	      if action.is_a?(Accept)
	        if opts[:accept] == :all
		  accepted << stack
	        else
		  v.puts("Accepting input.") if opts[:verbose]
                  if opts[:parse_tree]
		    (opts[:parse_tree] as IO).puts(stack.tree)
                  end

		  if (opts[:env] as Environment).he
 		    error = HandledError.new((opts[:env] as Environment).errors, stack.result as CLTK::Type)
                    raise error
		  else
		    return stack.result
		  end
	        end

	      elsif action.is_a?(Reduce)
	        # Get the production associated with this reduction.
	        production_proc, pop_size = @procs.not_nil![action.id]
	        if !production_proc
		  raise InternalParserException.new "No production #{action.id} found."
	        end
	        args, positions = stack.pop(pop_size)
	        (opts[:env] as Environment).set_positions(positions)

 	        if !production_proc.selections.empty?
                  new_args = [] of Type
                  production_proc.selections.each do |selection|
                    new_args = new_args + [ args[selection] ]
                  end
                  args = new_args
                end
                a = Array(Type).new
                args = args.each { |e| a << (e as Type)}

	        result = begin
                           production_proc.call(a as Array(Type), opts[:env])
		         end
	        if (goto = @states.not_nil![stack.state].on?(@lh_sides.not_nil![action.id]).first)

		  v.puts("Going to state #{goto.id}.\n") if opts[:verbose]
		  pos0 = nil
		  if args.empty?
		    # Empty productions need to be
		    # handled specially.
		    pos0 = stack.position

		    pos0.stream_offset	+= pos0.length + 1
		    pos0.line_offset	+= pos0.length + 1

		    pos0.length = 0
		  else
		    pos0 = (opts[:env] as Environment).pos( 0) as StreamPosition
		    pos1 = (opts[:env] as Environment).pos(-1) as StreamPosition
		    pos0.length = (pos1.stream_offset + pos1.length) - pos0.stream_offset
		  end
                  result = nil if result.is_a? Void
		  stack.push(goto.id, result, @lh_sides.not_nil![action.id], pos0)
	        else
		  raise InternalParserException.new "No GoTo action found in state #{stack.state} " +
					            "after reducing by production #{action.id}"
	        end

	        # This stack is NOT ready for the next
	        # token.
	        processing << stack

	        # Exit error mode if necessary.
	        error_mode = false if error_mode && !reduction_guard

	      elsif action.is_a?(Shift)
	        stack.push(action.id, token.value, token.type, token.position)
                # This stack is ready for the next
	        # token.
	        moving_on << stack

	        # Exit error mode.
	        error_mode = false
	      end
	    end
	  end

	  v.puts("\n\n") if opts[:verbose]

	  processing = moving_on
	  moving_on  = [] of ParseStack

	  # If we don't have any active stacks at this point the
	  # string isn't in the language.
	  if opts[:accept] == :first && processing.size == 0
            #	  v.close if v && v != $stdout
            #	  raise NotInLanguage.new(tokens[0...index], tokens[index], tokens[index+1..-1])
	    raise NotInLanguage.new(tokens[0...index], tokens[index], tokens[index+1..-1])

	  end

	  reduction_guard = false
        end

        # If we have reached this point we are accepting all parse
        # trees.
        if opts[:verbose]
	  v.puts("Accepting input with #{accepted.size} derivation(s).")

          #	v.close if v != $stdout
        end
        if opts[:parse_tree]?
             accepted.each do |stack|
	       (opts[:parse_tree] as IO).puts(stack.tree)
             end
        end
        results = accepted.map { |stack| stack.result as CLTK::Type}

        if (opts[:env] as Environment).he
	  raise HandledError.new((opts[:env] as Environment).errors, results as CLTK::Type)
        else
	  return results
        end
      end
    end
  end
end
