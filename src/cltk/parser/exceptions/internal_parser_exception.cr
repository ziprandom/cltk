module CLTK
  class Parser
    module Exceptions
      # Used for runtime exceptions that are the parsers fault.  These should
      # never be observed in the wild.
      class InternalParserException < Exception
        def initialize(message)
          @backtrace = [] of String
          @printable_backtrace = @backtrace
          super(message)
        end
      end
    end
  end
end
