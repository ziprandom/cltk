# coding: utf-8
module CLTK
  class CFG
    # Oddly enough, the Production class represents a production in a
    # context-free grammar.
    class Production
      # @return [Integer]  ID of this production.
      getter :id

      # @return [Symbol]  Left-hand side of this production.
      getter :lhs

      # @return [Array<Symbol>]  Right-hand side of this production.
      getter :rhs

      # Instantiates a new Production object with the specified ID,
      # and left- and right-hand sides.
      #
      # @param [Integer]        id   ID number of this production.
      # @param [Symbol]         lhs  Left-hand side of the production.
      # @param [Array<Symbol>]  rhs  Right-hand side of the production.
      def initialize(@id : Int32, @lhs : String, @rhs : Array(String)); end

      # Comparese on production to another.  Returns true only if the
      # left- and right- hand sides match.
      #
      # @param [Production]  other  Another production to compare to.
      #
      # @return [Boolean]
      def ==(other)
	self.id == other.id &&
          self.lhs == other.lhs &&
          self.rhs == other.rhs
      end

      # @return [Production]  A new copy of this production.
      def copy
	{{@type.id}}.new(@id, @lhs, @rhs)
      end

      # @return [Symbol]  The last terminal in the right-hand side of the production.
      def last_terminal
	@rhs.reduce(nil) do |m, sym|
          if CFG.is_terminal?(sym)
            sym
          else
            m
          end
        end
      end

      # @return [Item]  An Item based on this production.
      def to_item
	Item.new(0, @id, @lhs, @rhs)
      end

      # Returns a string representation of this production.
      #
      # @param [Integer]  padding  The ammount of padding spaces to add to the beginning of the string.
      #
      # @return [String]
      def to_s(padding = 0)
        if padding.is_a? Int32
	  (0..padding).map { " "}.join + "#{@lhs} -> " + (@rhs.empty? ? "ɛ" : @rhs.map { |s| s.to_s }.join(' '))
        end
      end
    end
  end
end
