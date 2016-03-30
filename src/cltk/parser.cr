# Description:	This file contains the base class for parsers that use CLTK.

############
# Requires #
############

# Ruby Language Toolkit
require "./cfg"
require "./ast"
#######################
# Classes and Modules #
#######################


class Array(T)
  def flatten
    arr = [] of T
    self.each do |el|
      if el.responds_to? :flatten
        el.flatten.each do |e|
          arr = arr + ([e] as Array)
        end
      else
        arr << el
      end
    end
    arr
  end
end


# The CLTK root module
module CLTK

  alias Type = ASTNode | Token | String | Symbol | Int32 | Float32 | Float64 | Bool | Nil | Array(Type) | Hash(String, Type)
  alias ValueType = Token | Int32 | String | Token | Nil | Array(ValueType)# | Hash(String, ValueType) #|
  # A BadToken error indicates that a token was observed in the input stream
  # that wasn't used in the grammar's definition.
  class BadToken < Exception
    def initialize()
      @backtrace = [] of String
      super(message)
    end

    # @return [String] String representation of the error.
    def to_s
      "Unexpected token.  Token not present in grammar definition."
    end
  end

  # A NotInLanguage error is raised whenever there is no valid parse tree
  # for a given token stream.  In other words, the input string is not in the
  # defined language.
  class NotInLanguage < Exception

    # @return [Array<Token>]  List of tokens that have been successfully parsed
    getter :seen

    # @return [Token]  Token that caused the parser to stop
    getter :current

    # @return [Array<Token>]  List of tokens that have yet to be seen
    getter :remaining

    # @param [Array<Token>]  seen       Tokens that have been successfully parsed
    # @param [Token]         current    Token that caused the parser to stop
    # @param [Array<Token>]  remaining  Tokens that have yet to be seen
    def initialize(@seen, @current, @remaining)
      super(message)
      @backtrace = [] of String
    end

    # @return [String] String representation of the error.
    def to_s
      "String not in language.  Token info:\n\tSeen: #{@seen}\n\tCurrent: #{@current}\n\tRemaining: #{@remaining}"
    end
  end

  # An error of this type is raised when the parser encountered a error that
  # was handled by an error production.
  class HandledError < Exception

    # The errors as reported by the parser.
    #
    # @return [Array<Object>]
    getter :errors

    # The result that would have been returned by the call to *parse*.
    getter :result

    # Instantiate a new HandledError object with *errors*.
    #
    # @param [Array<Object>]	errors Errors added to the parsing environment by calls to {Parser::Environment#error}.
    # @param [Object]		result Object resulting from parsing Tokens before the error occurred.
    def initialize(@errors, @result)
      @backtrace = [] of String
      super(message)
    end
  end

  # Used for exceptions that occure during parser construction.
  class ParserConstructionException < Exception
   def initialize(message)
      @backtrace = [] of String
      super(message)
   end
  end

  # Used for runtime exceptions that are the parsers fault.  These should
  # never be observed in the wild.
  class InternalParserException < Exception
    def initialize(message)
      @backtrace = [] of String
      @printable_backtrace = @backtrace
      super(message)
    end
  end

  # Used to indicate that a parser is empty or hasn't been finalized.
  class UselessParserException < Exception
    # Sets the error messsage for this exception.
    def initialize
      @backtrace = [] of String
      super("Parser has not been finalized.")
    end
  end

  # The Parser class may be sub-classed to produce new parsers.  These
  # parsers have a lot of features, and are described in the main
  # documentation.
  class Parser
    # @return [Environment] Environment used by the instantiated parser.
    getter :env

    #################
    # Class Methods #
    #################


    # The overridden new prevents un-finalized parsers from being
    # instantiated.
    def self.new(*args)
      if @@symbols.nil?
	raise UselessParserException.new
      else
	super(*args)
      end
    end

    # Installs instance class varialbes into a class.
    #
    # @return [void]
    macro inherited
      @@curr_lhs  = nil
      @@curr_prec = nil

      @@conflicts = Hash(Int32, Array({String, String})).new {|h, k| h[k] = Array({String, String}).new}

      @@grammar   = CLTK::CFG.new

      @@lh_sides  = {} of Int32 => String
      @@procs     = Hash(Int32, {ProdProc, Int32}).new #{|h, k| h[k] = Tuple.new}
      @@states    = Array(State).new

      # Variables for dealing with precedence.
      @@prec_counts      = {:left => 0, :right => 0, :non => 0}
      @@production_precs = {} of Int32 => (String | Nil | {String, Int32})
      @@token_precs      = {} of String => {String, Int32}
      @@token_hooks      = Hash(String, Array(Proc(Environment, Nil))).new {|h, k| h[k] = [] of Proc(Environment, Nil)}

      # Set the default argument handling policy.  Valid values
      # are :array and :splat.
      @@default_arg_type = :splat

      @@grammar.not_nil!.callback do |type, which, p, sels|
        proc = case type
	       when :optional
	         case which
	         when :empty then ProdProc.new { nil }
	         else             ProdProc.new { |o| o[0] }
	         end

	       when :elp
	         case which
	         when :empty then ProdProc.new { [] of CLTK::Type}
	         else             ProdProc.new { |prime| prime[0] }
	         end

	       when :nelp
	         case which
	         when :single
	           ProdProc.new { |el| [el[0]] }
	         when :multiple
	           ProdProc.new(:splat, sels) do |syms|
		     el = (syms as Array)[1..-1]
		     multiple = (syms.first as Array) << (el.size == 1 ? el.first : el)
                     multiple
	           end
	         else
	           ProdProc.new { |el| el = el as Array; el.size == 1 ? el.first : el }
	         end
	       end
	@@procs.not_nil![p.id.not_nil!] = {
          proc.not_nil!,
	  p.rhs.size
	}
	@@production_precs.not_nil![p.id] = p.last_terminal
        nil
      end
    end

    # If *state* (or its equivalent) is not in the state list it is
    # added and it's ID is returned.  If there is already a state
    # with the same items as *state* in the state list its ID is
    # returned and *state* is discarded.
    #
    # @param [State] state State to add to the parser.
    #
    # @return [Integer] The ID of the state.
    def self.add_state(state)
      id = @@states.not_nil!.index(state)
      if id
	id
      else
	state.id = @@states.not_nil!.size

	@@states.not_nil! << state
	@@states.not_nil!.size - 1
      end
    end

    # Build a hash with the default options for Parser.finalize
    # and then update it with the values from *opts*.
    #
    # @param [Hash{Symbol => Object}] opts Hash containing options for finalize.
    #
    # @return [Hash{Symbol => Object}]
    private def self.build_finalize_opts(opts)
      if opts.fetch(:explain).is_a? String
        val = self.get_io(opts[:explain])
      end
      if val
        opts[:explain]	= val as IO
      end
      {
	explain:    false,
	lookahead:  true,
	precedence: true,
	use:        false
      }.merge(opts)
    end

    # Build a hash with the default options for Parser.parse and
    # then update it with the values from *opts*.
    #
    # @param [Hash{Symbol => Object}] opts Hash containing options for parse.
    #
    # @return [Hash{Symbol => Object}]
    private def self.build_parse_opts(opts)
#      opts[:parse_tree] = self.get_io(opts[:parse_tree])
#      opts[:verbose]    = self.get_io(opts[:verbose])

      {
	accept:     :first,
	env:        (@@env || Environment).new as Environment,
	parse_tree: nil,
	verbose:    STDOUT
      }.merge(opts)
    end

    def self.setenv(env)
      @@env = env
    end

    # This method is used to (surprise) check the sanity of the
    # constructed parser.  It checks to make sure all non-terminals
    # used in the grammar definition appear on the left-hand side of
    # one or more productions, and that none of the parser's states
    # have invalid actions.  If a problem is encountered a
    # ParserConstructionException is raised.
    #
    # @return [void]
    def self.check_sanity
      # Check to make sure all non-terminals appear on the
      # left-hand side of some production.
      @@grammar.not_nil!.nonterms.each do |sym|
	unless @@lh_sides.not_nil!.values.includes?(sym)
	  raise ParserConstructionException.new "Non-terminal #{sym} does not appear on the left-hand side of any production."
	end
      end
      # Check the actions in each state.
      each_state do |state|
	state.actions.not_nil!.each do |sym, actions|
	  if CFG.is_terminal?(sym)
	    # Here we check actions for terminals.
	    actions.each do |action|
	      if action.is_a?(Accept)
		if sym.to_s != "EOS"
		  raise ParserConstructionException.new "Accept action found for terminal #{sym} in state #{state.id}."
		end

	      elsif !(action.is_a?(GoTo) || action.is_a?(Reduce) || action.is_a?(Shift))
		raise ParserConstructionException.new "Object of type #{action.class} found in actions for terminal " +
						   "#{sym} in state #{state.id}."

	      end
	    end

	    if (conflict = state.conflict_on?(sym))
	      self.inform_conflict(state.id, conflict, sym)
	    end
	  else
	    # Here we check actions for non-terminals.
	    if actions.size > 1
	      raise ParserConstructionException.new "State #{state.id} has multiple GoTo actions for non-terminal #{sym}."

	    elsif actions.size == 1 && !actions.first.is_a?(GoTo)
	      raise ParserConstructionException.new "State #{state.id} has non-GoTo action for non-terminal #{sym}."

	    end
	  end
	end
      end
    end

    # This method checks to see if the parser would be in parse state
    # *dest* after starting in state *start* and reading *symbols*.
    #
    # @param [Symbol]         start    Symbol representing a CFG production.
    # @param [Symbol]         dest     Symbol representing a CFG production.
    # @param [Array<Symbol>]  symbols  Grammar symbols.
    #
    # @return [Boolean] If the destination symbol is reachable from the start symbol after reading *symbols*.
    def self.check_reachability(start, dest, symbols)
      path_exists = true
      cur_state   = start

      symbols.each do |sym|

	actions = @@states.not_nil![cur_state.id.not_nil!].on?(sym)
	actions = actions.select { |a| a.is_a?(Shift) } if CFG.is_terminal?(sym)

	if actions.empty?
	  path_exists = false
	  break
	end

	# There can only be one Shift action for terminals and
	# one GoTo action for non-terminals, so we know the
	# first action is the only one in the list.
	cur_state = @@states.not_nil![actions.first.id.not_nil!]
      end

      path_exists && cur_state.id == dest.id
    end

    # Declares a new clause inside of a production.  The right-hand
    # side is specified by *expression* and the precedence of this
    # production can be changed by setting the *precedence* argument
    # to some terminal symbol.
    #
    # @param [String, Symbol]  expression  Right-hand side of a production.
    # @param [Symbol]          precedence  Symbol representing the precedence of this production.
    # @param [:array, :splat]  arg_type    Method to use when passing arguments to the action.
    # @param [Proc]            action      Action to be taken when the production is reduced.
    #
    # @return [void]
    def self.clause(expression, precedence = nil, arg_type = @@default_arg_type, &action: Array(Type), Environment -> _)
      # Use the curr_prec only if it isn't overridden for this
      # clause.
      precedence ||= @@curr_prec
      production, selections = if @@grammar
                                 e = (@@grammar as CFG).clause(expression)
                                 e
                               else
                                 raise "NO GRAMMAR DEFINED"
                               end
      # Check to make sure the action's arity matches the number
      # of symbols on the right-hand side.
      expected_arity = (selections.empty? ? production.rhs.size : selections.size)
      if arg_type == :splat && action.arity != expected_arity
#	raise ParserConstructionException.new "Incorrect number of action parameters.  Expected #{expected_arity} but got #{action.arity}." + " Action arity must match the number of terminals and non-terminals in the clause."
        #puts "Incorrect number of action parameters.  Expected #{expected_arity} but got #{action.arity}." + " Action arity must match the number of terminals and non-terminals in the clause."
      end
      # Add the action to our proc list.
      @@procs.not_nil![production.id] = {
        ProdProc.new(arg_type, selections, &action),
        production.rhs.size
      }

      # If no precedence is specified use the precedence of the
      # last terminal in the production.
      @@production_precs.not_nil![production.id] = (precedence || production.last_terminal)
    end

    def self.c(expression, precedence = nil, arg_type = @@default_arg_type, &action: Array(Type), Environment -> _)
      self.clause(expression, precedence, arg_type, &action)
    end
    # Removes resources that were needed to generate the parser but
    # aren't needed when actually parsing input.
    #
    # @return [void]
    def self.clean
      # We've told the developer about conflicts by now.
      @@conflicts = nil

      # Drop the grammar and the grammar'.
      @@grammar       = nil
      @@grammar_prime = nil

      # Drop precedence and bookkeeping information.
      @@cur_lhs  = nil
      @@cur_prec = nil

      @@prec_counts      = nil
      @@production_precs = nil
      @@token_precs      = nil

      # Drop the items from each of the states.
      each_state { |state| state.clean }
    end

    # Set the default argument type for the actions associated with
    # clauses.  All actions defined after this call will be passed
    # arguments in the way specified here, unless overridden in the
    # call to {Parser.clause}.
    #
    # @param [:array, :splat] type The default argument type.
    #
    # @return [void]
    def self.default_arg_type(type)
      @@default_arg_type = type if type == :array || type == :splat
    end

    def self.dat(type)
      self.default_arg_type(type)
    end
    # Adds productions and actions for parsing empty lists.
    #
    # @see CFG#empty_list_production
    def self.build_list_production(symbol, list_elements, separator = "")
      if list_elements.is_a? Array
        list_elements = list_elements.map {|e| e.to_s}
      else
        list_elements = list_elements.to_s
      end
      @@grammar.not_nil!.build_list_production(symbol, list_elements, separator.to_s)
    end

    def self.list(symbol, list_elements, separator = "")
      self.build_list_production(symbol, list_elements, separator)
    end
    # This function will print a description of the parser to the
    # provided IO object.
    #
    # @param [IO] io Input/Output object used for printing the parser's explanation.
    #
    # @return [void]
    def self.explain(io : IO)
      if @@grammar && !@@states.not_nil!.empty?
	io.puts("###############")
	io.puts("# Productions #")
	io.puts("###############")
	io.puts

	max_id_length = @@grammar.not_nil!.productions(:id).not_nil!.size.to_s.size

	# Print the productions.
	@@grammar.not_nil!.productions.not_nil!.each do |sym, productions|
          productions = productions as Array(CLTK::CFG::Production)
	  max_rhs_length = (productions).reduce(0) do |m, p|
            if (len = p.to_s.not_nil!.size) > m
              len
            else
              m
            end
          end

	  productions.not_nil!.each do |production|
	    p_string = production.to_s

            #	      io.print("\tProduction #{sprintf("%#{max_id_length}d", production.id)}: #{p_string}")
            prec = @@production_precs.not_nil![production.id];
	    if (prec.is_a?({Int32, String}))
	      io.print(" " * (max_rhs_length - p_string.not_nil!.size))
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

	max_token_len = @@grammar.not_nil!.terms.reduce(0) do |m, t|
          if t.size > m
            t.size
          else m
          end
        end

	@@grammar.not_nil!.terms.to_a.sort {|a,b| a.to_s <=> b.to_s }.each do |term|
	  io.print("\t#{term}")

	  if (prec = @@token_precs.not_nil![term])
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

	io.puts("\tStart symbol: #{@@grammar.not_nil!.start_symbol}'")
	io.puts

	io.puts("\tTotal number of states: #{@@states.not_nil!.size}")
	io.puts

	io.puts("\tTotal conflicts (maybe wrong - flatten impl): #{@@conflicts.not_nil!.values.flatten.size}")
	io.puts

	@@conflicts.not_nil!.each do |state_id, conflicts|
	  io.puts("\tState #{state_id} has #{@@conflicts.not_nil!.size} conflict(s)")
	end
        @@conflicts = @@conflicts.not_nil!
	io.puts unless @@conflicts.not_nil!.empty?

	# Print the parse table.
	io.puts("###############")
	io.puts("# Parse Table #")
	io.puts("###############")
	io.puts

	each_state do |state|
	  io.puts("State #{state.id}:")
	  io.puts

	  io.puts("\t# ITEMS #")
	  max = state.items.not_nil!.reduce(0) do |max, item|
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

	  state.actions.not_nil!.keys.sort {|a,b| a.to_s <=> b.to_s}.each do |sym|
	    state.actions.not_nil![sym].each do |action|
	      io.puts("\tOn #{sym} #{action}")
	    end
	  end

	  io.puts
	  io.puts("\t# CONFLICTS #")

	  if @@conflicts.not_nil![state.id.not_nil!].size == 0
	    io.puts("\tNone\n\n")
	  else
	    @@conflicts.not_nil![state.id.not_nil!].each do |conflict|
	      type, sym = conflict

	      io.print("\t#{if type == :SR; "Shift/Reduce"; else "Reduce/Reduce"; end} conflict")

	      io.puts(" on #{sym}")
	    end

	    io.puts
	  end
	end

	# Close any IO objects that aren't $stdout.
	if io.is_a?(IO)
          if io != $stdout
            #io.close
          end
        end
      else
	raise ParserConstructionException.new "Parser.explain called outside of finalize."
      end
    end

    # This method will finalize the parser causing the construction
    # of states and their actions, and the resolution of conflicts
    # using lookahead and precedence information.
    #
    # No calls to {Parser.production} may appear after the call to
    # Parser.finalize.
    #
    # @param [Hash] opts Options describing how to finalize the parser.
    #
    # @option opts [Boolean,String,IO]  :explain     To explain the parser or not.
    # @option opts [Boolean]            :lookahead   To use lookahead info for conflict resolution.
    # @option opts [Boolean]            :precedence  To use precedence info for conflict resolution.
    # @option opts [String,IO]          :use         A file name or object that is used to load/save the parser.
    #
    # @return [void]
    def self.finalize(opts : Hash(Symbol, Bool | String | IO) = {lookahead: true, precedence: true} )
      if (@@grammar.not_nil!.productions(:sym) as Hash(String, Array(CLTK::CFG::Production))).empty?
	#raise ParserConstructionException,
	raise Exception.new "Parser has no productions.  Cowardly refusing to construct an empty parser."
      end

      # Get the full options hash.
#      if (opts.is_a? Hash(Symbol, Bool | String ))
#        opts = {
#          lookahead: true,
#	  precedence: true
#        }.merge opts
#      end
      # Get the name of the file in which the parser is defined.
      #
      # FIXME: See why this is failing for the simple ListParser example.
      def_file = caller()[2].split(':')[0] if opts.has_key? :use

      # Check to make sure we can load the necessary information
      # from the specified object.
      if opts.has_key? :use
        raise Exception.new "reading the parser from a file is not yet supported"
#        && (
#	   (opts[:use].is_a?(String) && File.exists?(opts[:use] as String) #&& File::Stat.mtime(opts[:use]) > File.mtime(def_file)
#           ) ||
#	   (opts[:use].is_a?(File) #&& opts[:use].mtime > File.mtime(def_file)
#           )
#	 )
#
#	file = self.get_io(opts[:use], 'r')
#
#	# Un-marshal our saved data structures.
##	file.flock(File::LOCK_SH)
#	@lh_sides, @states, @symbols = Marshal.load(file)
##	file.flock(File::LOCK_UN)
#
#	# Close the file if we opened it.
#	file.close if opts[:use].is_a?(String)
#
#	# Remove any un-needed data and return.
#	return self.clean
      end

      # Grab all of the symbols that comprise the grammar
      # (besides the start symbol).
      @@symbols = @@grammar.not_nil!.symbols.to_a + ["ERROR"]
      # Add our starting state to the state list.
      @@start_symbol       = (@@grammar.not_nil!.start_symbol.to_s + "\'")
      start_production    = @@grammar.not_nil!.production(@@start_symbol, @@grammar.not_nil!.start_symbol).first
      start_state         = State.new(@@symbols, [start_production.to_item])
      start_state.close(@@grammar.not_nil!.productions(:sym) as Hash(String, Array(CLTK::CFG::Production)))
      self.add_state(start_state)

      # Translate the precedence of productions from tokens to
      # (associativity, precedence) pairs.
      @@production_precs = @@production_precs.not_nil!.map { |key, prec| @@token_precs.not_nil![prec]? as Nil | String | {String, Int32}}
      # Build the rest of the transition table.
      each_state do |state|
        # Transition states.
        tstates = Hash(String, State).new {|h,k| h[k] = State.new(@@symbols) }

	#Bin each item in this set into reachable transition
	#states.

	state.each do |item|
	  if (next_symbol = item.next_symbol)
            unless tstates[next_symbol]?
              tstates[next_symbol] = State.new(@@symbols)
            end
            tstates[next_symbol] << item.copy
	  end
	end
	# For each transition state:
	#  1) Get transition symbol
	#  2) Advance dot
	#  3) Close it
	#  4) Get state id and add transition
	tstates.each do |symbol, tstate|
	  tstate.each { |item| item.advance }

	  tstate.close(@@grammar.not_nil!.productions(:sym) as Hash(String, Array(CLTK::CFG::Production)))

	  id = self.add_state(tstate)

	  # Add Goto and Shift actions.
	  state.on(symbol, CFG.is_nonterminal?(symbol) ? GoTo.new(id) : Shift.new(id))
	end

	# Find the Accept and Reduce actions for this state.
	state.each do |item|
	  if item.at_end?
	    if item.lhs == @@start_symbol
	      state.on("EOS", Accept.new)
	    else
	      state.add_reduction(
                (@@grammar.not_nil!.productions(:id) as Hash(Int32, CLTK::CFG::Production))[item.id]
              )
	    end
	  end
	end
      end

      # Build the production.id -> production.lhs map.
      @@grammar.not_nil!.productions(:id).not_nil!.each do |id, production|
        @@lh_sides.not_nil![id as Int32] = (production as CLTK::CFG::Production).not_nil!.lhs
      end

      # Prune the parsing table for unnecessary reduce actions.
      self.prune(opts[:lookahead]?, opts[:precedence]?)

      # Check the parser for inconsistencies.
      self.check_sanity

      # Print the table if requested.
      exp = opts[:explain]?
      if exp.is_a? IO
        self.explain(exp)
      end

      # Remove any data that is no longer needed.
      self.clean
      # Store the parser's final data structures if requested.
      if opts[:use]?
        raise Exception.new "storing the parser to a file is not yet supported"
        #	io = self.get_io(opts[:use])
        #
        #	io.flock(File::LOCK_EX) if io.is_a?(File)
        #	Marshal.dump([@lh_sides, @states, @symbols], io)
        #	io.flock(File::LOCK_UN) if io.is_a?(File)
        #
        #	# Close the IO object if we opened it.
        #	io.close if opts[:use].is_a?(String)
      end
    end

    # Converts an object into an IO object as appropriate.
    #
    # @param [Object]  o     Object to be converted into an IO object.
    # @param [String]  mode  String representing the mode to open the IO object in.
    #
    # @return [IO, false] The IO object or false if a conversion wasn't possible.
    def self.get_io(o, mode = "w")
      if o.is_a?(Bool)
        STDOUT
      elsif o.is_a?(String)
	File.open(o, mode).read
      elsif o.is_a?(IO)
	o
      else
	false
      end
    end

    # Iterate over the parser's states.
    #
    # @yieldparam [State]  state  One of the parser automaton's state objects
    #
    # @return [void]
    def self.each_state
      current_state = 0
      while current_state < @@states.not_nil!.size
	yield @@states.not_nil!.at(current_state)
	current_state += 1
      end
    end

    # @return [CFG]  The grammar that can be parsed by this Parser.
    def self.grammar
      @@grammar.clone
    end

    # This method generates and memoizes the G' grammar used to
    # calculate the LALR(1) lookahead sets.  Information about this
    # grammar and its use can be found in the following paper:
    #
    # Simple Computation of LALR(1) Lookahead Sets
    # Manuel E. Bermudez and George Logothetis
    # Information Processing Letters 31 - 1989
    #
    # @return [CFG]
    def self.grammar_prime
      unless @@grammar_prime
	@@grammar_prime = CFG.new

	each_state do |state|
	  state.each do |item|
	    lhs = "#{state.id}_#{item.next_symbol}".to_s

	    next unless CFG.is_nonterminal?(item.next_symbol) &&
                        !(@@grammar_prime.not_nil!.productions(:sym) as Hash(String, Array(CLTK::CFG::Production)))
                          .keys.includes?(lhs)

	    (@@grammar.not_nil!.productions(:sym) as Hash(String, Array(CLTK::CFG::Production)))
              .not_nil![item.next_symbol.not_nil!].each do |production|
	      rhs = ""

	      cstate = state

	      production.rhs.each do |symbol|
		rhs += "#{cstate.id}_#{symbol} "

		cstate = @@states.not_nil![cstate.on?(symbol).first.id.not_nil!]
	      end

	      @@grammar_prime.not_nil!.production(lhs, rhs)
	    end
	  end
	end
      end

      @@grammar_prime
    end

    # Inform the parser core that a conflict has been detected.
    #
    # @param [Integer]   state_id  ID of the state where the conflict was encountered.
    # @param [:RR, :SR]  type      Reduce/Reduce or Shift/Reduce conflict.
    # @param [Symbol]    sym       Symbol that caused the conflict.
    #
    # @return [void]
    def self.inform_conflict(state_id, type, sym)
      @@conflicts.not_nil![state_id.not_nil!] << {type.to_s, sym}
    end

    # This method is used to specify that the symbols in *symbols*
    # are left-associative.  Subsequent calls to this method will
    # give their arguments higher precedence.
    #
    # @param [Array<Symbol>]  symbols  Symbols that are left associative.
    #
    # @return [void]
    def self.left(*symbols)
      prec_level = @@prec_counts.not_nil![:left] += 1

      symbols.map do |sym|
	@@token_precs.not_nil![sym.to_s] = {:left.to_s, prec_level}
      end
    end

    # This method is used to specify that the symbols in *symbols*
    # are non-associative.
    #
    # @param [Array<Symbol>]  symbols  Symbols that are non-associative.
    #
    # @return [void]
    def self.nonassoc(*symbols)
      prec_level = @prec_counts[:non] += 1

      symbols.map { |s| s.to_sym }.each do |sym|
	@token_precs[sym] = [:non, prec_level]
      end
    end

    # Adds productions and actions for parsing nonempty lists.
    #
    # @see CFG#nonempty_list_production
    def self.build_nonempty_list_production(symbol, list_elements, separator = "")
      if list_elements.is_a? Array
        list_elements = list_elements.map do |e|
          if e
            e.to_s
          else
            ""
          end
        end
      else
        list_elements = list_elements.to_s
      end
      @@grammar.not_nil!.build_nonempty_list_production(symbol, list_elements, separator.to_s)
    end

    def self.nonempty_list(symbol, list_elements, separator = "")
      self.build_nonempty_list_production(symbol, list_elements, separator)
    end
    # This function is where actual parsing takes place.  The
    # _tokens_ argument must be an array of Token objects, the last
    # of which has type EOS.  By default this method will return the
    # value computed by the first successful parse tree found.
    #
    # Additional information about the parsing options can be found in
    # the main documentation.
    #
    # @param [Array<Token>]  tokens  Tokens to be parsed.
    # @param [Hash]          opts    Options to use when parsing input.
    #
    # @option opts [:first, :all]       :accept      Either :first or :all.
    # @option opts [Object]             :env         The environment in which to evaluate the production action.
    # @option opts [Boolean,String,IO]  :parse_tree  To print parse trees in the DOT language or not.
    # @option opts [Boolean,String,IO]  :verbose     To be verbose or not.
    #
    # @return [Object, Array<Object>]  Result or results of parsing the given tokens.
    def self.parse(tokens, opts = {} of Symbol => (Symbol))
      # Get the full options hash.
      opts = build_parse_opts({ verbose: false }.merge(opts))
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
        unless @@symbols.not_nil!.includes?(token.type.to_s)
	  raise BadToken.new
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
	  @@token_hooks.not_nil![token.type.to_s].each { |hook| hook.call(opts[:env] as Environment)}

	  # Get the available actions for this stack.
	  actions = @@states.not_nil![stack.state as Int32].on?(token.type.to_s)
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
	        if (actions = @@states.not_nil![stack.state].on?(:ERROR.to_s)).empty?
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
		  raise HandledError.new((opts[:env] as Environment).errors, stack.result)
		else
		  return stack.result
		end
	      end

	    elsif action.is_a?(Reduce)
	      # Get the production associated with this reduction.
	      production_proc, pop_size = @@procs.not_nil![action.id]
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
	      if (goto = @@states.not_nil![stack.state].on?(@@lh_sides.not_nil![action.id]).first)

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
		stack.push(goto.id, result, @@lh_sides.not_nil![action.id], pos0)
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

      results = accepted.map { |stack| stack.result }

      if (opts[:env] as Environment).he
	raise HandledError.new((opts[:env] as Environment).errors, results)
      else
	return results
      end
    end

    # Adds a new production to the parser with a left-hand value of
    # *symbol*.  If *expression* is specified it is taken as the
    # right-hand side of the production and *action* is associated
    # with the production.  If *expression* is nil then *action* is
    # evaluated and expected to make one or more calls to
    # Parser.clause.  A precedence can be associate with this
    # production by setting *precedence* to a terminal symbol.
    #
    # @param [Symbol]				symbol		Left-hand side of the production.
    # @param [String, Symbol, nil]	expression	Right-hand side of the production.
    # @param [Symbol, nil]			precedence	Symbol representing the precedence of this produciton.
    # @param [:array, :splat]		arg_type		Method to use when passing arguments to the action.
    # @param [Proc]				action		Action associated with this production.
    #
    # @return [void]
    def self.production(symbol, expression = nil, precedence = nil, arg_type = @@default_arg_type, &action: Array(Type), Environment -> _)

      # Check the symbol.
      if !(symbol.is_a?(Symbol) || symbol.is_a?(String)) || !CFG.is_nonterminal?(symbol)
	raise ParserConstructionException.new "Production symbols must be Strings or Symbols and be in all lowercase."
      end

      @@grammar.not_nil!.curr_lhs = symbol
      @@curr_prec        = precedence

      orig_dat = nil
      if arg_type != @@default_arg_type
	orig_dat = @@default_arg_type
	@@default_arg_type = arg_type
      end
      if expression
	self.clause(expression, precedence, @@default_arg_type, &action)
      else
        with self yield [] of Type, Environment.new
      end

      @@default_arg_type = orig_dat if !orig_dat.nil?

      @@grammar.not_nil!.curr_lhs = nil
      @@curr_prec        = nil
    end

    def self.p(symbol, expression = nil, precedence = nil, arg_type = @@default_arg_type, &action: Array(Type), Environment -> _)
      self.production(symbol, expression, precedence, arg_type, &action)
    end

    def self.p(symbol, expression = nil, precedence = nil, arg_type = @@default_arg_type, &action: Array(Type), Environment -> _)
      self.production(symbol, expression, precedence, arg_type, &action)
    end
    # This method uses lookahead sets and precedence information to
    # resolve conflicts and remove unnecessary reduce actions.
    #
    # @param [Boolean]  do_lookahead   Prune based on lookahead sets or not.
    # @param [Boolean]  do_precedence  Prune based on precedence or not.
    #
    # @return [void]
    def self.prune(do_lookahead, do_precedence)
      terms = @@grammar.not_nil!.terms

      # If both options are false there is no pruning to do.
      return if !(do_lookahead || do_precedence)

      each_state do |state0|

	#####################
	# Lookahead Pruning #
	#####################

	if do_lookahead
	  # Find all of the reductions in this state.
	  reductions = state0.actions.not_nil!.values.flatten.uniq.select { |a| a.is_a?(Reduce) }
          # reduction is ok ..
	  reductions.each do |reduction|
            raction_id = (reduction as Action).id.not_nil!
	    production = (@@grammar.not_nil!.productions(:id) as Hash(Int32, CLTK::CFG::Production))[raction_id]
	    lookahead = Array(String).new

	    # Build the lookahead set.
	    each_state do |state1|
	      if self.check_reachability(state1, state0, production.rhs)
		lookahead |= self.grammar_prime.not_nil!.follow_set("#{state1.id}_#{production.lhs}".to_s)
	      end
	    end

	    # Translate the G' follow symbols into G
	    # lookahead symbols.
	    lookahead = lookahead.map { |sym| sym.to_s.split('_', 2).last }.uniq

	    # Here we remove the unnecessary reductions.
	    # If there are error productions we need to
	    # scale back the amount of pruning done.
	    pruning_candidates = terms.to_a - lookahead

	    if terms.includes?("ERROR")
	      pruning_candidates.each do |sym|
		state0.actions.not_nil![sym].delete(reduction) if state0.conflict_on?(sym)
	      end
	    else
	      pruning_candidates.each { |sym| state0.actions.not_nil![sym].delete(reduction) }
	    end
	  end
	end

	########################################
	# Precedence and Associativity Pruning #
	########################################

	if do_precedence
	  state0.actions.not_nil!.each do |symbol, actions|

	    # We are only interested in pruning actions
	    # for terminal symbols.
	    next unless CFG.is_terminal?(symbol)

	    # Skip to the next one if there is no
	    # possibility of a Shift/Reduce or
	    # Reduce/Reduce conflict.
	    next unless actions && actions.size > 1

	    resolve_ok = actions.reduce(true) do |m, a|
	      if a.is_a?(Reduce)
		m && @@production_precs.not_nil![a.id.not_nil!]
	      else
		m
	      end
	    end && actions.reduce(false) { |m, a| m  || a.is_a?(Shift) }

	    if @@token_precs.not_nil!.has_key?(symbol) && @@token_precs.not_nil![symbol] && resolve_ok
	      max_prec = 0
	      selected_action = nil
	      # Grab the associativity and precedence
	      # for the input token.
	      tassoc, tprec = @@token_precs.not_nil![symbol]

	      actions.each do |a|
		assoc, prec = (a.is_a?(Shift) ? {tassoc, tprec} : @@production_precs.not_nil![a.id.not_nil!] ) as {String, Int32}

		# If two actions have the same precedence we
		# will only replace the previous production if:
		#  * The token is left associative and the current action is a Reduce
		#  * The token is right associative and the current action is a Shift
		if prec > max_prec  || (prec == max_prec && tassoc == (a.is_a?(Shift) ? :right : :left))
		  max_prec        = prec
		  selected_action = a

		elsif prec == max_prec && assoc == :nonassoc
		  raise ParserConstructionException.new "Non-associative token found during conflict resolution."

		end
	      end

	      state0.actions.not_nil![symbol] = [selected_action.not_nil! as Action]
	    end
	  end
	end
      end
    end

    # This method is used to specify that the symbols in _symbols_
    # are right associative.  Subsequent calls to this method will
    # give their arguments higher precedence.
    #
    # @param [Array<Symbol>] symbols Symbols that are right-associative.
    #
    # @return [void]
    def self.right(*symbols)
      prec_level = @@prec_counts.not_nil![:right] += 1

      symbols.map do |sym|
	@@token_precs.not_nil![sym.to_s] = {:right.to_s, prec_level}
      end
    end

    # Changes the starting symbol of the parser.
    #
    # @param [Symbol] symbol The starting symbol of the grammar.
    #
    # @return [void]
    def self.start(symbol)
      @grammar.start symbol
    end

    # Add a hook that is executed whenever *sym* is seen.
    #
    # The *sym* must be a terminal symbol.
    #
    # @param [Symbol]  sym   Symbol to hook into
    # @param [Proc]    proc  Code to execute when the block is seen
    #
    # @return [void]
    def self.token_hook(sym, &proc: Proc(Environment, Nil))
      if CFG.is_terminal?(sym)
	@@token_hooks.not_nil![sym.to_s] << proc
      else
	raise "Method token_hook expects `sym` to be non-terminal."
      end
    end


    ####################
    # Instance Methods #
    ####################

    # Instantiates a new parser and creates an environment to be
    # used for subsequent calls.
    def initialize
      @env = Environment.new
    end

    # Parses the given token stream using the encapsulated environment.
    #
    # @see .parse
    def parse(tokens, opts)
      self.class.parse(tokens, {:env => @env}.update(opts))
    end

    ################################

    # All actions passed to Parser.producation and Parser.clause are
    # evaluated inside an instance of the Environment class or its
    # subclass (which must have the same name).
    class Environment
      # Indicates if an error was encountered and handled.
      #
      # @return [Boolean]
      property :he

      # A list of all objects added using the *error* method.
      #
      # @return [Array<Object>]
      getter :errors

      # Instantiate a new Environment object.
      def initialize
        @errors = [] of Type
        self.reset
      end

      # Adds an object to the list of errors.
      #
      # @return [void]
      def error(o)
        @errors << o
      end

      # Returns a StreamPosition object for the symbol at location n,
      # indexed from zero.
      #
      # @param [Integer] n Index for symbol position.
      #
      # @return [StreamPosition] Position of symbol at index n.
      def pos(n)
        if @positions
          @positions.not_nil![n] as StreamPosition
        end
      end

      # Reset any variables that need to be re-initialized between
      # parse calls.
      #
      # @return [void]
      def reset
        @errors	= [] of Type
        @he		= false
      end

      # Setter for the *positions* array.
      #
      # @param [Array<StreamPosition>] positions
      #
      # @return [Array<StreamPosition>] The same array of positions.
      def set_positions(positions)
        @positions = positions
      end
    end

    # The ParseStack class is used by a Parser to keep track of state
    # during parsing.
    class ParseStack
      # @return [Integer] ID of this parse stack.
      getter :id

      # @return [Array<Object>] Array of objects produced by {Reduce} actions.
      getter :output_stack

      # @return [Array<Integer>] Array of states used when performing {Reduce} actions.
      getter :state_stack

      # Instantiate a new ParserStack object.
      #
      # @param [Integer]                id           ID for this parse stack.  Used by GLR algorithm.
      # @param [Array<Object>]          ostack       Output stack.  Holds results of {Reduce} and {Shift} actions.
      # @param [Array<Integer>]         sstack       State stack.  Holds states that have been shifted due to {Shift} actions.
      # @param [Array<Integer>]         nstack       Node stack.  Holds dot language IDs for nodes in the parse tree.
      # @param [Array<Array<Integer>>]  connections  Integer pairs representing edges in the parse tree.
      # @param [Array<Symbol>]          labels       Labels for nodes in the parse tree.
      # @param [Array<StreamPosition>]  positions    Position data for symbols that have been shifted.
      def initialize(id,
                     ostack = [] of Type,
                     sstack = [0] of Int32,
                     nstack = [] of Int32,
                     connections = [] of {Int32, Int32},
                     labels = [] of String,
                     positions = [] of StreamPosition)
        @id = id

        @node_stack   = nstack
        @output_stack = ostack
        @state_stack  = sstack

        @connections  = connections
        @labels       = labels
        @positions    = positions
      end

      # Branch this stack, effectively creating a new copy of its
      # internal state.
      #
      # @param [Integer] new_id ID for the new ParseStack.
      #
      # @return [ParseStack]
      def branch(new_id)
        # We have to do a deeper copy of the output stack to avoid
        # interactions between the Proc objects for the different
        # parsing paths.
        #
        # The being/rescue block is needed because some classes
        # respond to `clone` but always raise an error.
        new_output_stack = [] of Type
        @output_stack.each do |o|
	  # Check to see if we can obtain a deep copy.
	  if o.responds_to?(:copy)
	    new_output_stack.push o.copy as Type
	  else
	    begin
              new_output_stack.push o.clone as Type
            rescue
              new_output_stack.push o as Type
            end
	  end
        end

        ParseStack.new(new_id, new_output_stack, @state_stack.clone,
		       @node_stack.clone, @connections.clone, @labels.clone, @positions.clone)
      end

      # @return [StreamPosition] Position data for the last symbol on the stack.
      def position
        if @positions.empty?
	  StreamPosition.new
        else
	  @positions.last.clone
        end
      end

      # Push new state and other information onto the stack.
      #
      # @param [Integer]			state	ID of the shifted state.
      # @param [Object]			o		Value of Token that caused the shift.
      # @param [Symbol]			node0	Label for node in parse tree.
      # @param [StreamPosition]	position	Position token that got shifted.
      #
      # @return [void]
      def push(state, o, node0, position)
        @state_stack << state.not_nil!
        if o.is_a? Array
          a = [] of Type as Type
          o.each { |e| (a as Array(Type)).push(e as Type) }
          @output_stack = @output_stack + [a]
        elsif o.is_a? Type
          @output_stack << o
        else
          @output_stack = @output_stack + [o]
        end

        @node_stack	<< @labels.size
        @labels		<< if CFG.is_terminal?(node0) && o
                             node0.to_s + "(#{o})"
                           else
                             node0.to_s
                           end as String
      @positions	<< position.not_nil!

      if CFG.is_nonterminal?(node0)
        @cbuffer.not_nil!.each do |node1|
	  @connections << {@labels.size - 1, node1}
        end
      end
    end

    # Pop some number of objects off of the inside stacks.
    #
    # @param [Integer] n Number of object to pop off the stack.
    #
    # @return [Array(Object, StreamPosition)] Values popped from the output and positions stacks.
    def pop(n = 1)
      @state_stack.pop(n)
      # Pop the node stack so that the proper edges can be added
      # when the production's left-hand side non-terminal is
      # pushed onto the stack.
      @cbuffer = @node_stack.pop(n)
      {@output_stack.pop(n), @positions.pop(n)}
    end

    # Fetch the result stored in this ParseStack.  If there is more
    # than one object left on the output stack there is an error.
    #
    # @return [Object] The end result of this parse stack.
    def result
      if @output_stack.size == 1
        return @output_stack.last
      else
        raise InternalParserException.new "The parsing stack should have 1 element on the output stack, not #{@output_stack.size}."
      end
    end

    # @return [Integer] Current state of this ParseStack.
    def state
      @state_stack.last
    end

    # @return [String] Representation of the parse tree in the DOT langauge.
    def tree
      tree  = "digraph tree#{@id} {\n"

      @labels.each_with_index do |label, i|
        tree += "\tnode#{i} [label=\"#{label}\""

        if CFG.is_terminal?(label)
	  tree += " shape=box"
        end

        tree += "];\n"
      end

      tree += "\n"

      @connections.each do |from, to|
        tree += "\tnode#{from} -> node#{to};\n"
      end

      tree += "}"
    end
  end

  # The State class is used to represent sets of items and actions to be
  # used during parsing.
  class State
    # @return [Integer] State's ID.
    property :id

    # @return  [Array<CFG::Item>]  Item objects that comprise this state
    getter :items

    # @return [Hash{Symbol => Array<Action>}]  Maps lookahead symbols to actions
    getter :actions

    # Instantiate a new State object.
    #
    # @param [Array<Symbol>]     tokens  Tokens that represent this state
    # @param [Array<CFG::Item>]  items   Items that make up this state
    def initialize(tokens : Array(String)?, items = [] of CFG::Item)
      @id      = nil
      @items   = items
      if tokens
        @actions = tokens.reduce({} of String => Array(CLTK::Parser::Action) ) { |h, t| h[t] = Array(CLTK::Parser::Action).new; h }
      end
    end

    # Compare one State to another.  Two States are equal if they
    # have the same items or, if the items have been cleaned, if
    # the States have the same ID.
    #
    # @param [State]  other  Another State to compare to
    #
    # @return [Boolean]
    def ==(other)
      if self.items && other.items
        self.items == other.items
      else
        self.id == other.id
      end
    end

    # Add a Reduce action to the state.
    #
    # @param [Production]  production  Production used to perform the reduction
    #
    # @return [void]
    def add_reduction(production)
      action = Reduce.new(production)

      # Reduce actions are not allowed for the ERROR terminal.
      @actions.not_nil!.each do |k, v|
        if CFG.is_terminal?(k) && k.to_s != "ERROR"
          v << action
        end
      end
    end

    # @param [CFG::Item] item Item to add to this state.
    def append(item)
      if item.is_a?(CFG::Item) &&  !@items.not_nil!.includes?(item)
        @items.not_nil! << item
      end
    end

    def <<(item)
      append(item)
    end
    # Clean this State by removing the list of {CFG::Item} objects.
    #
    # @return [void]
    def clean
      @items = nil
    end

    # Close this state using *productions*.
    #
    # @param [Array<CFG::Production>] productions Productions used to close this state.
    #
    # @return [vod]
    def close(productions)
      self.each do |item|
        next_symbol = item.next_symbol
        if next_symbol && CFG.is_nonterminal?(next_symbol)
	  productions.not_nil![next_symbol].each { |p| self << p.to_item }
        end
      end
    end

    # Checks to see if there is a conflict in this state, given a
    # input of *sym*.  Returns :SR if a shift/reduce conflict is
    # detected and :RR if a reduce/reduce conflict is detected.  If
    # no conflict is detected nil is returned.
    #
    # @param [Symbol] sym Symbol to check for conflicts on.
    #
    # @return [:SR, :RR, nil]
    def conflict_on?(sym)

      reductions	= 0
      shifts		= 0

      @actions.not_nil![sym].each do |action|
        if action.is_a?(Reduce)
	  reductions += 1

        elsif action.is_a?(Shift)
	  shifts += 1

        end
      end

      if shifts == 1 && reductions > 0
        :SR
      elsif reductions > 1
        :RR
      else
        nil
      end
    end

    # Iterate over the state's items.
    #
    # @return [void]
    def each
      current_item = 0
      while current_item < @items.not_nil!.size
        yield @items.not_nil!.at(current_item)
        current_item += 1
      end
    end

    # Specify an Action to perform when the input token is *symbol*.
    #
    # @param [Symbol] symbol Symbol to add action for.
    # @param [Action] action Action for symbol.
    #
    # @return [void]
    def on(symbol, action)
      if @actions.not_nil!.has_key?(symbol.to_s)
        @actions.not_nil![symbol.to_s] = (@actions.not_nil![symbol.to_s] as Array(Action)) << action as Action
      else
        raise ParserConstructionException.new "Attempting to set action for token (#{symbol}) not seen in grammar definition."
      end
    end

    # Returns that actions that should be taken when the input token
    # is *symbol*.
    #
    # @param [Symbol] symbol Symbol we want the actions for.
    #
    # @return [Array<Action>] Actions that should be taken.
    def on?(symbol)
      @actions.not_nil![symbol].clone
    end
  end

#  alias ProdProc = Proc
  # A subclass of Proc that indicates how it should be passed arguments
  # by the parser.
  struct ProdProc
    # @return [:array, :splat]  Method that should be used to pass arguments to this proc.
    getter :arg_type

    # @return [Array<Integer>]  Mask for selection of tokens to pass to action.  Empty mask means pass all.
    getter :selections

    def initialize(arg_type = :splat, selections = [] of Int32, &block: Array(Type), Environment -> _)
#      super(&block)
      @block = block
      @arg_type   = arg_type
      @selections = selections
    end
    def call(args, env)
      @block.call(args as Array(Type), env as Environment)
    end
  end

# The Action class is used to indicate what action the parser should
# take given a current state and input token.
class Action
  # @return [Integer] ID of this action.
  getter :id

  # @param [Integer] id ID of this action.
  def initialize(id = nil)
    @id = id
  end
end

# The Accept class indicates to the parser that it should accept the
# current parse tree.
class Accept < Action
  # @return [String] String representation of this action.
  def to_s
    "Accept"
  end
end

# The GoTo class indicates to the parser that it should goto the state
# specified by GoTo.id.
class GoTo < Action
  # @return [String] String representation of this action.
  def to_s
    "GoTo #{self.id}"
  end
end

# The Reduce class indicates to the parser that it should reduce the
# input stack by the rule specified by Reduce.id.
class Reduce < Action

  # @param [Production]  production  Production to reduce by
  def initialize(production)
    super(production.id)

    @production = production
  end

  # @return [String] String representation of this action.
  def to_s
    "Reduce by Production #{self.id} : #{@production.to_s}"
  end
end

# The Shift class indicates to the parser that it should shift the
# current input token.
class Shift < Action
  # @return [String] String representation of this action.
  def to_s
    "Shift to State #{self.id}"
  end
end
end
end
