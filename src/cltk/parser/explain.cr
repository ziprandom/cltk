module CLTK::Parser::Explain
  # This function will print a description of the parser to the
  # provided IO object.
  #
  # @param [IO] io Input/Output object used for printing the parser's explanation.
  #
  # @return [void]
  def self.explain(io : IO)
    if @@grammar && !@@states.empty?
      io.puts("###############")
      io.puts("# Productions #")
      io.puts("###############")
      io.puts

      max_id_length = @@grammar.productions_id.size.to_s.size

      # Print the productions.
      @@grammar.productions.each do |sym, productions|
        productions = productions.as(Array(CLTK::CFG::Production))
	max_rhs_length = (productions).reduce(0) do |m, p|
          if (len = p.to_s.size) > m
            len
          else
            m
          end
        end

	productions.each do |production|
	  p_string = production.to_s

          #	      io.print("\tProduction #{sprintf("%#{max_id_length}d", production.id)}: #{p_string}")
          prec = @@production_precs_prepare[production.id];
	  if (prec.is_a?({Int32, String}))
	    io.print(" " * (max_rhs_length - p_string.size))
	    io.print(" : (#{sprintf("%-5s", prec.first)}, #{prec.last})")
	  end

	  io.puts
	end

	io.puts
      end

      io.puts("##########")
      io.puts("# Tokens #")
      io.puts("##########")
      io.puts

      max_token_len = @@grammar.terms.reduce(0) do |m, t|
        if t.size > m
          t.size
        else m
        end
      end

      @@grammar.terms.to_a.sort {|a,b| a.to_s <=> b.to_s }.each do |term|
	io.print("\t#{term}")

	if (prec = @@token_precs[term])
	  io.print(" " * (max_token_len - term.size))
	  io.print(" : (#{sprintf("%-5s", prec.first)}, #{prec.last})")
	end

	io.puts
      end

      io.puts

      io.puts("#####################")
      io.puts("# Table Information #")
      io.puts("#####################")
      io.puts

      io.puts("\tStart symbol: #{@@grammar.start_symbol}'")
      io.puts

      io.puts("\tTotal number of states: #{@@states.size}")
      io.puts

      io.puts("\tTotal conflicts (maybe wrong - flatten impl): #{@@conflicts.values.flatten.size}")
      io.puts

      @@conflicts.each do |state_id, conflicts|
	io.puts("\tState #{state_id} has #{@@conflicts.size} conflict(s)")
      end
      @@conflicts = @@conflicts
      io.puts unless @@conflicts.empty?

      # Print the parse table.
      io.puts("###############")
      io.puts("# Parse Table #")
      io.puts("###############")
      io.puts

      each_state do |state|
	io.puts("State #{state.id}:")
	io.puts

	io.puts("\t# ITEMS #")
	max = state.items.reduce(0) do |max, item|
	  if item.lhs.to_s.size > max
            item.lhs.to_s.size
          else
            max
          end
	end

	state.each do |item|
	  io.puts("\t#{item.to_s(max)}")
	end

	io.puts
	io.puts("\t# ACTIONS #")

	state.actions.keys.sort {|a,b| a.to_s <=> b.to_s}.each do |sym|
	  state.actions[sym].each do |action|
	    io.puts("\tOn #{sym} #{action}")
	  end
	end

	io.puts
	io.puts("\t# CONFLICTS #")

	if @@conflicts[state.id].size == 0
	  io.puts("\tNone\n\n")
	else
	  @@conflicts[state.id].each do |conflict|
	    type, sym = conflict

	    io.print("\t#{if type == :SR; "Shift/Reduce"; else "Reduce/Reduce"; end} conflict")

	    io.puts(" on #{sym}")
	  end

	  io.puts
	end
      end

      # Close any IO objects that aren't $stdout.
      if io.is_a?(IO)
        if io != STDOUT
          #io.close
        end
      end
    else
      #raise ParserConstructionException.new "Parser.explain called outside of finalize."
    end
  end
end
