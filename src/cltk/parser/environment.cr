module CLTK
  class Parser
    ################################

    # All actions passed to Parser.producation and Parser.clause are
    # evaluated inside an instance of the Environment class or its
    # subclass (which must have the same name).
    class Environment
      # Indicates if an error was encountered and handled.
      #
      # @return [Boolean]
      property :he

      # A list of all objects added using the *error* method.
      #
      # @return [Array<Object>]
      getter :errors

      # Instantiate a new Environment object.
      def initialize
        @errors = [] of Type
        self.reset
      end

      def yield_with_self
        with self yield
      end

      # Adds an object to the list of errors.
      #
      # @return [void]
      def error(o)
        @errors << o
      end

      # Returns a StreamPosition object for the symbol at location n,
      # indexed from zero.
      #
      # @param [Integer] n Index for symbol position.
      #
      # @return [StreamPosition] Position of symbol at index n.
      def pos(n)
        if @positions
          @positions.not_nil![n] as StreamPosition
        end
      end

      # Reset any variables that need to be re-initialized between
      # parse calls.
      #
      # @return [void]
      def reset
        @errors	= [] of Type
        @he	= false
      end

      # Setter for the *positions* array.
      #
      # @param [Array<StreamPosition>] positions
      #
      # @return [Array<StreamPosition>] The same array of positions.
      def set_positions(positions)
        @positions = positions
      end
    end
  end
end
