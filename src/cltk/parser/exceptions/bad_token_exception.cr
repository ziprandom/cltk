module CLTK
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
end
