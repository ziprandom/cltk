module CLTK
  class LexerOld
    # The Rule class is used simply for data encapsulation.
    struct Rule
      # @return [Proc] Token producting action to be taken when this rule is matched.
      getter :action

      # @return [Regexp] Regular expression for matching this rule.
      getter :pattern

      # @return [Array<Symbol>] Flags currently set in this lexing environment.
      getter :flags

      # Instantiates a new Rule object.
      #
      # @param [Regexp]		pattern	Regular expression used to match to this rule.
      # @param [Proc]		action	Token producing action associated with this rule.
      # @param [Symbol]		state	State in which this rule is active.
      # @param [Array<Symbol>]	flags	Flags that must be enabled for this rule to match.

      def initialize(@pattern : Regex, @state : Symbol, @flags : Array(Symbol),
                     &@action : Proc(Regex::MatchData, String, CLTK::Lexer::Environment, BlockReturn))
      end
    end

  end
end
