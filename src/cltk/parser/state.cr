module CLTK
  class Parser
    # The State class is used to represent sets of items and actions to be
    # used during parsing.
    class State
      # @return [Integer] State's ID.
      property :id

      # @return  [Array<CFG::Item>]  Item objects that comprise this state
      getter :items

      # @return [Hash{Symbol => Array<Action>}]  Maps lookahead symbols to actions
      getter :actions

      @id = -1
      @actions : Hash(String, Array(CLTK::Parser::Action))

      def initialize(@id : Int32, @actions : Hash(String, Array(CLTK::Parser::Action)), @items : Array(CFG::Item)); end
      # Instantiate a new State object.
      #
      # @param [Array<Symbol>]     tokens  Tokens that represent this state
      # @param [Array<CFG::Item>]  items   Items that make up this state
      def initialize(tokens : Array(String), @items = [] of CFG::Item)
        @actions = tokens.reduce(
          {} of String => Array(CLTK::Parser::Action)
        ) do |h, t|
          h.tap {
            h[t] = Array(CLTK::Parser::Action).new
          }
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
        self.items == other.items ||
          self.id == other.id
      end

      # Add a Reduce action to the state.
      #
      # @param [Production]  production  Production used to perform the reduction
      #
      # @return [void]
      def add_reduction(production)
        action = Actions::Reduce.new(production)

        # Reduce actions are not allowed for the ERROR terminal.
        @actions.each do |k, v|
          if CFG.is_terminal?(k) && k.to_s != "ERROR"
            v << action
          end
        end
      end

      # @param [CFG::Item] item Item to add to this state.
      def append(item)
        if item.is_a?(CFG::Item) &&  !@items.includes?(item)
          @items << item
        end
      end

      def <<(item)
        append(item)
      end
      # Clean this State by removing the list of {CFG::Item} objects.
      #
      # @return [void]
      def clean
        @items = [] of CFG::Item
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
	    productions[next_symbol].each { |p| self << p.to_item }
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
        actions = @actions[sym]?

        return nil unless actions

        reductions	= 0
        shifts		= 0

        actions.each do |action|
          if action.is_a?(Actions::Reduce)
	    reductions += 1

          elsif action.is_a?(Actions::Shift)
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
        while current_item < @items.size
          yield @items[current_item]
          current_item += 1
        end
      end

      # Specify an Action to perform when the input token is *symbol*.
      #
      # @param [Symbol] symbol Symbol to add action for.
      # @param [Action] action Action for symbol.
      #
      # @return [void]
      def on(symbol, action : Action)
        if @actions.has_key?(symbol.to_s)
          @actions[symbol.to_s] = @actions[symbol.to_s] << action
        else
          raise Exception.new "Attempting to set action for token (#{symbol}) not seen in grammar definition."
        end
      end

      # Returns that actions that should be taken when the input token
      # is *symbol*.
      #
      # @param [Symbol] symbol Symbol we want the actions for.
      #
      # @return [Array<Action>] Actions that should be taken.
      def on?(symbol)
        actions = @actions[symbol]?
        if actions
          actions.map { |a| a.dup }
        else
          [] of Action
        end
      end
    end

  end
end
