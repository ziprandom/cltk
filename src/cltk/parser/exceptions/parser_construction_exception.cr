module CLTK
  class Parser
    module Exceptions
      # Used for exceptions that occure during parser construction.
      class ParserConstructionException < Exception
        def initialize(message)
          @backtrace = [] of String
          super(message)
        end
      end
    end
  end
end
