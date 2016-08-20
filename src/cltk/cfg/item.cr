# coding: utf-8
module CLTK
  class CFG
    # The Item class represents a CFG production with dot in it.
    class Item < Production
      # @return [Integer] Index of the next symbol in this item.
      getter :dot

      # Instantiates a new Item object with a dot located before the
      # symbol at index *dot* of the right-hand side.  The remaining
      # arguments (*args*) should be as specified by
      # {Production#initialize}.
      #
      # @param [Integer]        dot   Location of the dot in this Item.
      # @param [Array<Object>]  args  (see {Production#initialize})
      def initialize(dot : Int32, *args)
        super(*args)

        # The Dot indicates the NEXT symbol to be read.
        @dot = dot
      end

      # Compares two items.
      #
      # @param [Item]  other  Another item to compare to.
      #
      # @return [Boolean]
      def ==(other : Item)
        self.dot == other.dot && self.lhs == other.lhs && self.rhs == other.rhs
      end

      # Moves the items dot forward by one if the end of the right-hand
      # side hasn't already been reached.
      #
      # @return [Integer, nil]
      def advance
        if @dot < @rhs.size
          @dot += 1
        end
      end

      # Tests to see if the dot is at the end of the right-hand side.
      #
      # @return [Boolean]
      def at_end?
        @dot == @rhs.size
      end

      # @return [Item]  A new copy of this item.
      def copy
        Item.new(@dot, @id, @lhs, @rhs.clone)
      end

      # Returns the symbol located after the dot.
      #
      # @return [Symbol]  Symbol located after the dot (at the index indicated by the {#dot} attribute).
      def next_symbol
        if @dot < @rhs.size
          @rhs[@dot]
        else
          nil
        end
      end

      # Returns a string representation of this item.
      #
      # @param [Integer]  padding  The ammount of padding spaces to add to the beginning of the string.
      #
      # @return [String]
      def to_s(padding = 0)
        Array(String).new(padding){" "}.join + @lhs + "->" + @rhs.map { |s| s.to_s }.insert(@dot, "·").join(' ')
      end
    end
  end
end
