# coding: utf-8
require "./lexers/ebnf"
require "./cfg/production"
require "./cfg/item"

class Object
  def in?(collection : Array | Set)
    collection.includes? self
  end
end

module CLTK

  # The CFG class is used to represent context-free grammars.  It is used by
  # the CLTK::Parser class to represent the parser's grammar, but can also be
  # used to manipulate arbitrary CFGs.
  class CFG

    # An exception class that represents a problem with a context-free
    # grammar's definition.
    class GrammarError < Exception; end

    alias ProductionBufferType   = NamedTuple( production: Production, selections: Array(Int32) )
    alias ProductionCallbackType = Proc(Symbol, Symbol, Production, Array(Int32), Nil)

    @callback            = ProductionCallbackType.new {}
    @start_symbol        = ""
    @curr_lhs            : String?                          =  nil
    @lexer               = Lexers::EBNF.new
    @production_buffer   = Array(ProductionBufferType).new
    @terms               = Set(String).new(["EOS"])
    @nonterms            = Set(String).new
    @firsts              = Hash(String, Array(String)).new
    @follows             = Hash(String, Array(String)).new
    @production_counter  = -1
    @productions_id      = Hash(Int32, Production).new
    @productions_sym     = Hash( String, Array(Production) ).new do |h, k|
      h[k] = [] of Production
    end

    property :curr_lhs
    getter :start_symbol
    getter :productions_sym
    getter :productions_id

    # @return [Integer]  ID for the next production to be defined.
    def next_id
      @production_counter += 1
    end

    def production(symbol : Symbol | String, expression : String? = nil, &block)
      with_curr_lhs(symbol) do
        if expression
          self.clause(expression)
        else
          with self yield
	  @production_buffer.dup
        end
      end
    end

    def production(symbol : Symbol | String, expression : String? = nil)
      with_curr_lhs(symbol) do
        clause(expression)
      end
    end

    def with_curr_lhs(symbol)
      @production_buffer = [] of ProductionBufferType
      prev_lhs  = @curr_lhs
      @curr_lhs = symbol.to_s
      ret_val = yield
      @curr_lhs = prev_lhs
      return ret_val
    end

    def clause( expression : Symbol | String )
      unless @curr_lhs
        raise GrammarError.new "CFG#clause called outside of CFG#production block."
      end

      lhs        = @curr_lhs.to_s
      rhs        = [] of String
      tokens     = @lexer.lex(expression.to_s)
      selections = [] of Int32

      # Set this as the start symbol if there isn't one already
      # defined.
      @start_symbol = @start_symbol.empty? ? lhs : @start_symbol
      # Remove EBNF tokens and replace them with new productions.
      symbol_count = 0
      tokens.each_with_index do |token, i|
        if token.type.in? [:TERM, :NONTERM]
	  # Add this symbol to the correct collection.
	  if token.type == :TERM
            @terms << token.value.as(String)
          else
            @nonterms << token.value.as(String)
          end

          if (next_token = tokens[i + 1]?)
            tvalue = token.value.to_s.downcase
            rhs << case next_token.type
	           when :QUESTION then
                     self.get_optional_production(
                       "#{tvalue}_optional", token.value.as(String))
	           when :STAR     then
                     self.get_list_production(
                       "#{tvalue}_list", token.value.as(String))
	           when :PLUS     then
                     self.get_nonempty_list_production(
                       "#{tvalue}_nonempty_list",token.value.as(String))
	           else token.value.as(String)
                   end
            symbol_count += 1
          else
	    rhs << token.value.as(String)
          end
        elsif token.type == :DOT
	  selections << symbol_count
        end
      end

      # Make the production.
      production = Production.new(self.next_id, lhs, rhs)
      @production_buffer << {
        production: production,
        selections: selections
      }

      # Make sure the production symbol is collected.
      @nonterms << lhs

      # Add the new production to our collections.
      self.add_production(production)
      return { production: production, selections: selections }
    end

    def get_optional_production(name : String, list_elements : String | Array(String))
      if name.in? @nonterms
	name
      else
	build_optional_production(name, list_elements)
      end
    end

    def get_list_production(name, list_elements : String | Array(String), separator = "")
      if name.in? @nonterms
	name
      else
	build_list_production(name, list_elements, separator)
      end
    end

    def get_nonempty_list_production(name : String, list_elements : String | Array(String), separator = "")
      if name.in? @nonterms
	name
      else
	build_nonempty_list_production(name, list_elements, separator = "")
      end
    end

    def build_optional_production(name : String, opt_symbol : String)
      return name if @productions_sym.has_key?(name)
      # Add the items for the following productions:
      #
      # name: | opt_symbol

      # Empty production.
      production = self.add_production(
        Production.new(self.next_id, name, [] of String)
      )

      @callback.call(:optional, :empty, production, [] of Int32)

      # Nonempty production
      production = self.add_production(
        Production.new(self.next_id, name, [opt_symbol])
      )
      @callback.call(:optional, :nonempty, production, [] of Int32)

      # Add the new symbol to the list of nonterminals.
      @nonterms << name
      name
    end

    def build_list_production(name : String, list_elements : String | Array(String), separator = "")
      # Add the items for the following productions:
      #
      # name: | name_prime
      name_prime = "#{name}_prime"

      # 1st Production
      production = self.production(name, "")[:production]
      @callback.call(:elp, :empty, production, [] of Int32)

      # 2nd Production
      production = self.production(name, name_prime)[:production]
      @callback.call(:elp, :nonempty, production, [] of Int32)

      # Add remaining productions via nonempty_list helper.
      self.build_nonempty_list_production(name_prime, list_elements, separator)

      name
    end

    def build_nonempty_list_production(name : String, list_elements : String | Array(String), separator = "")
      # Add the items for the following productions:
      #
      # If there is only one list element:
      #
      #   name: list_element | name separator list_element
      #
      # else
      #
      #   name: name_list_elements | name separator name_list_elements
      #
      #   name_list_elements: #{list_elements.join('|')}
      build_elements_productions = false

      list_element_string =
        if list_elements.is_a?(Array)
	  if list_elements.empty?
	    raise ArgumentError.new "Parameter list_elements must not be empty."
	  elsif list_elements.size == 1
	    list_elements.first
	  else
	    build_elements_productions = true
	    "#{name}_list_elements"
	  end
	else
	  list_elements
	end

      list_element_selected_string = list_element_string.split.map { |s| ".#{s}" }.join(' ')

      # Single Element Production
      production = self.production(name, list_element_string)[:production]
      @callback.call(:nelp, :single, production, [] of Int32)

      # Multiple Element Production
      production, selections = self.production(name, ".#{name} #{separator} #{list_element_selected_string}").values
      @callback.call(:nelp, :multiple, production, selections)
      if build_elements_productions
	# List Element Productions
        if list_elements.is_a? Array
	  list_elements.each do |element|
	    production = self.production(list_element_string, element)[:production]
	    @callback.call(:nelp, :elements, production , [] of Int32)
	  end
        end
      end
      name
    end

    # Adds *production* to the appropriate internal data structures.
    #
    # @param [Production] production The production to add to the grammar.
    #
    # @return [void]
    def add_production(production)
      @productions_id[production.id] = production
      @productions_sym[production.lhs] << production
      production
    end

    def callback(&callback : Symbol, Symbol, Production, Array(Int32)-> Nil)
      @callback = callback
      nil
    end

    # @return [Array<Symbol>]  All symbols used in the grammar's definition.
    def symbols
      self.terms | self.nonterms
    end

    def terms
      @terms.clone
    end

    def nonterms
      @nonterms.clone
    end

    def self.is_terminal?(sym)
      sym && (s = sym.to_s) == s.upcase
    end

    def self.is_nonterminal?(sym)
      sym && sym.to_s == sym.to_s.downcase
    end

    def first_set(sentence : String | Array(String) ) : Array(String)
      if sentence.is_a?(String)
	first_set_prime(sentence)
      else
        sentence.is_a? Array &&
                       sentence.reduce(true) { |m, sym| m && self.symbols.includes?(sym) }
	set0 = [] of String
	all_have_empty = true

	sentence.each do |sym|
	  set0 |= (set1 = self.first_set(sym)) - ["ɛ"]

	  break unless (all_have_empty = set1.includes?("ɛ"))
	end

	if all_have_empty
          set0 + ["ɛ"]
        else
          set0
        end
      end
    end

    private def first_set_prime(sym0 : String, seen_lh_sides : Array(String) = [] of String) : Array(String)
      if sym0.in? self.symbols
	# Memoize the result for later.
	@firsts[sym0] ||=
	  if CFG.is_terminal?(sym0)
	    # If the symbol is a terminal, it is the only symbol in
	    # its follow set.
	    [sym0]
	  else
	    set0 = [] of String

	    @productions_sym[sym0].each do |production|
	      if production.rhs.empty?
		# If this is an empty production we should
		# add the empty string to the First set.
		set0 << "ɛ"
	      else
		all_have_empty = true

		production.rhs.each do |sym1|

		  set1 = [] of Symbol

		  # Grab the First set for the current
		  # symbol in this production.
		  unless seen_lh_sides.includes?(sym1)
		    set0 |= (set1 = first_set_prime(sym1, seen_lh_sides << sym1)) - ["ɛ"]
		  end
                  unless (all_have_empty = set1.includes?("ɛ"))
		    break
                  end
		end

		# Add the empty production if this production
		# is all non-terminals that can be reduced to
		# the empty string.
		set0 << "ɛ" if all_have_empty
	      end
	    end

	    set0.uniq
	  end
      else
	[] of String
      end
    end

    def follow_set(sym0 : String, seen_lh_sides = [] of String) : Array(String)
      # Use the memoized set if possible.
      return @follows[sym0] if @follows.has_key?(sym0)
      if @nonterms.includes? sym0
	set0 = [] of String

	# Add EOS to the start symbol's follow set.
	set0 << "EOS" if sym0 == @start_symbol
	@productions_id.values.each do |production|
	  production.rhs.each_with_index do |sym1, i|
	    if i + 1 < production.rhs.size
	      if sym0 == sym1
		set0 |= (set1 = self.first_set(production.rhs[(i + 1)..-1])) - ["ɛ"]
		set0 |= self.follow_set(production.lhs) if set1.includes?("ɛ")
	      end
	    elsif sym0 != production.lhs && sym0 == sym1 && !seen_lh_sides.includes?(production.lhs)
	      set0 |= self.follow_set(production.lhs, seen_lh_sides << production.lhs)
	    end
	  end
	end
	if seen_lh_sides.empty? || !set0.empty?
	  # Memoize the result for later.
	  @follows[sym0] ||= set0
	else
	  set0
	end
      else
	[] of String
      end
    end
  end
end
