module CLTK
  class Parser
    # A subclass of Proc that indicates how it should be passed arguments
    # by the parser.
    struct ProdProc
      # @return [:array, :splat]  Method that should be used to pass arguments to this proc.
      getter :arg_type

      # @return [Array<Integer>]  Mask for selection of tokens to pass to action.  Empty mask means pass all.
      getter :selections

      def initialize(arg_type = :splat, selections = [] of Int32, &block: Array(Type), Environment -> _)
        @block = block
        @arg_type   = arg_type
        @selections = selections
      end
      def call(args, env)
        @block.call(args as Array(Type), env as Environment)
      end
    end
  end
end
