module CLTK
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

    def initialize(@errors : Array(CLTK::Type), @result : CLTK::Type )
      @backtrace = [] of String
      super("HandledError")
    end
  end
end
