module CLTK
  # Used to indicate that a parser is empty or hasn't been finalized.
  class UselessParserException < Exception
    # Sets the error messsage for this exception.
    def initialize
      @backtrace = [] of String
      super("Parser has not been finalized.")
    end
  end
end
