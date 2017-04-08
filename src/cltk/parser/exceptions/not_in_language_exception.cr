module CLTK
  class Parser
    module Exceptions
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
        def initialize(@seen : Array(CLTK::Token), @current : CLTK::Token, @remaining : Array(CLTK::Token))
          super(message)
          @backtrace = [] of String
        end

        # @return [String] String representation of the error.
        def to_s
          "String not in language.  Token info:\n\tSeen: #{@seen}\n\tCurrent: #{@current}\n\tRemaining: #{@remaining}"
        end
      end
    end
  end
end
