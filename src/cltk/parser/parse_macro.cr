macro def_parse(params_as_const = true)

  def self.parse(tokens : Array, opts : NamedTuple? = nil)
    {% if params_as_const %}
      _parse(PROCS, LH_SIDES, SYMBOLS, STATES, TOKEN_HOOKS, tokens, opts)
    {% else %}
      _parse(@@procs, @@lh_sides, @@symbols, @@states, @@token_hooks, tokens, opts)
    {% end %}
  end

  private def self.build_parse_opts(opts : NamedTuple?)
    {
      accept:     :first,
      env:        {% if params_as_const %}Environment{% else %}@@env{% end %}.new,
      parse_tree: nil,
      verbose:    nil,
    }.merge(opts)
  end

  def self._parse(procs, lh_sides, symbols, states, token_hooks, tokens, opts : NamedTuple? = nil)
    # Get the full options hash.
    opts = build_parse_opts(opts)
    {% if env("VERBOSE") %}
      v = STDOUT
      v.puts("Input tokens:")
      v.puts(tokens.map { |t| t.type }.inspect)
      v.puts
    {% end %}

      # Stack IDs to keep track of them during parsing.
      stack_id = 0

    # Error mode indicators.
    error_mode      = false
    reduction_guard = false

    # Our various list of stacks.
    accepted   = [] of CLTK::Parser::ParseStack
    moving_on  = [] of CLTK::Parser::ParseStack
    processing = [CLTK::Parser::ParseStack.new(stack_id += 1)]
    # Iterate over the tokens.  We don't procede to the
    # next token until every stack is done with the
    # current one.
    tokens.each_with_index do |token, index|
      # Check to make sure this token was seen in the
      # grammar definition.
      unless symbols.includes?(token.type.to_s)
	raise CLTK::BadToken.new(token)
      end

      {% if env("VERBOSE") %}
        st = if token.value
               "(" + token.value.to_s.as(String) + ")"
             else
               ""
             end
	v.puts("Current token: #{token.type}#{st}")
      {% end %}

        # Iterate over the stacks until each one is done.
        while (processing.any?)
          stack = processing.shift
          # Execute any token hooks in this stack's environment.
          token_hooks.fetch(
            token.type.to_s, [] of Proc(Environment, Nil)
          ).each { |hook| hook.call(opts[:env]) }

          # Get the available actions for
          actions = states[stack.state].on?(token.type.to_s)
          if actions.empty?
            # If we are already in error mode and there
            # are no actions we skip this token.
            if error_mode
              {% if env("VERBOSE") %}
                st = if token.value
                       "(" + token.value.to_s.as(String) + ")"
                     end
	        v.puts("Discarding token: #{token.type}#{st}") if v
              {% end %}
                # Add the current token to the array
                # that corresponds to the output value
                # for the ERROR token.
                stack.output_stack.last.as(Array(CLTK::Type)) << token.value.as(CLTK::Type)
              moving_on << stack
              next
            end

            # We would be dropping the last stack so we
            # are going to go into error mode.
            if accepted.empty? && moving_on.empty? && processing.empty?
              {% if env("VERBOSE") %}
	        v.puts
	        v.puts("Current stack:")
	        v.puts("\tID: #{stack.id}")
	        v.puts("\tState stack:\t#{stack.state_stack.inspect}")
	        v.puts("\tOutput Stack:\t#{stack.output_stack.inspect}")
	        v.puts
              {% end %}

	        # Try and find a valid error state.
	        while stack.state_stack.any?
	          if (actions = states[stack.state].on?(:ERROR.to_s)).empty?
	            # This state doesn't have an
	            # error production. Moving on.
                    stack.pop
	          else
	            # Enter the found error state.
	            stack.push(actions.first.id, [token.value.as(CLTK::Type)], :ERROR.to_s, token.position)
	            break
	          end
	        end
	      if stack.state_stack.any?
	        # We found a valid error state.
	        error_mode = reduction_guard = true
	        opts[:env].he = true
	        moving_on << stack

                {% if env("VERBOSE") %}
	          v.puts("Invalid input encountered.  Entering error handling mode.")
	          v.puts("Discarding token: #{token.type}(#{token.value})")
                {% end %}
	      else
		# No valid error states could be
		# found.  Time to print a message
		# and leave.
                {% if env("VERBOSE") %}
                  v.puts("No more actions for stack #{stack.id}.  Dropping stack.") if v
                {% end %}
	      end
	    else
              {% if env("VERBOSE") %}
	        v.puts("No more actions for stack #{stack.id}.  Dropping stack.") if v
              {% end %}
	    end

	    next
	  end

	  # Make (stack, action) pairs, duplicating the
	  # stack as necessary.
	  pairs = ([{stack, actions.pop}] + actions.map {|action| {stack.branch(stack_id += 1), action} })
	  pairs.each do |pair|
            stack, action = pair

            {% if env("VERBOSE") %}
	      v.puts
	      v.puts("Current stack:")
	      v.puts("\tID: #{stack.id}")
	      v.puts("\tState stack:\t#{stack.state_stack.inspect}")
	      v.puts("\tOutput Stack:\t#{stack.output_stack.inspect}")
	      v.puts
	      v.puts("Action taken: #{action.to_s}")
	    {% end %}

	      if action.is_a?(CLTK::Parser::Accept)
	        if opts[:accept] == :all
		  accepted << stack
	        else
                  {% if env("VERBOSE") %}
		    v.puts("Accepting input.")
                  {% end %}
                    if opts[:parse_tree]
		      opts[:parse_tree].as(IO).puts(stack.tree)
                    end

		  if opts[:env].he
 		    error = CLTK::HandledError.new(
                      opts[:env].errors, stack.result
                    )
                    raise error
		  else
		    return stack.result
		  end
	        end

	      elsif action.is_a?(CLTK::Parser::Reduce)
	        # Get the production associated with this reduction.
	        production_proc, pop_size = procs[action.id]
	        if !production_proc
		  raise CLTK::InternalParserException.new "No production #{action.id} found."
	        end
	        args, positions = stack.pop(pop_size)
	        opts[:env].set_positions(positions)

 	        if !production_proc.selections.empty?
                  args = production_proc.selections.map do |selection|
                    args[selection].as(CLTK::Type)
                  end
                end

	        result = begin
                           production_proc.call(args, opts[:env])
		         end
	        if (goto = states[stack.state].on?(lh_sides[action.id]).first)
                  {% if env("VERBOSE") %}
		    v.puts("Going to state #{goto.id}.\n")
                  {% end %}
		    pos0 = nil
		  if args.empty?
		    # Empty productions need to be
		    # handled specially.
		    pos0 = stack.position

		    pos0.stream_offset	+= pos0.length + 1
		    pos0.line_offset	+= pos0.length + 1

		    pos0.length = 0
		  else
		    pos0 = opts[:env].pos( 0).as(CLTK::StreamPosition)
		    pos1 = opts[:env].pos(-1).as(CLTK::StreamPosition)
		    pos0.length = (pos1.stream_offset + pos1.length) - pos0.stream_offset
		  end
                  result = nil if result.is_a? Void
		  stack.push(goto.id, result, lh_sides[action.id], pos0)
	        else
		  raise CLTK::InternalParserException.new "No GoTo action found in state #{stack.state} " +
					                  "after reducing by production #{action.id}"
	        end

	        # This stack is NOT ready for the next
	        # token.
	        processing << stack

	        # Exit error mode if necessary.
	        error_mode = false if error_mode && !reduction_guard

	      elsif action.is_a?(CLTK::Parser::Shift)
	        stack.push(action.id, token.value, token.type, token.position)
                # This stack is ready for the next
	        # token.
	        moving_on << stack

	        # Exit error mode.
	        error_mode = false
	      end
	  end
	end

      {% if env("VERBOSE") %}
	v.puts("\n\n")
      {% end %}

      processing = moving_on
      moving_on  = [] of CLTK::Parser::ParseStack

      # If we don't have any active stacks at this point the
      # string isn't in the language.
      if opts[:accept] == :first && processing.size == 0
        {% if env("VERBOSE") %}
          v.close unless v == STDOUT
        {% end%}
	  raise CLTK::NotInLanguage.new(tokens[0...index], tokens[index], tokens[index+1..-1])

      end

      reduction_guard = false
    end

    # If we have reached this point we are accepting all parse
    # trees.
    {% if env("VERBOSE") %}
      v.puts("Accepting input with #{accepted.size} derivation(s).")
      v.close unless v == STDOUT
    {% end %}

      if opts[:parse_tree]?
           accepted.each do |stack|
	     opts[:parse_tree].as(IO).puts(stack.tree)
           end
      end

    results = accepted.map { |stack| stack.result.as(CLTK::Type) }

    if (opts[:env]).he
      raise CLTK::HandledError.new(opts[:env].as(Environment).errors, results.as(CLTK::Type))
    else
      return results
    end
  end
end
