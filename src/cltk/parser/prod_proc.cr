module CLTK
  class Parser
    # A subclass of Proc that indicates how it should be passed arguments
    # by the parser.
    struct ProdProc
      # @return [:array, :splat]  Method that should be used to pass arguments to this proc.
      getter :arg_type

      # @return [Array<Integer>]  Mask for selection of tokens to pass to action.  Empty mask means pass all.
      getter :selections
      @selections : Array(Int32)
      @block : Proc(Array(CLTK::Type), Environment, Symbol, Type)? = nil

      def initialize(@arg_type = :splat, @selections = [] of Int32)
        @block = nil
      end

      def initialize(
                     @arg_type = :splat,
                     @selections = [] of Int32,
                                         &block : Proc(Array(CLTK::Type), Environment, Symbol, Type))
        @block = block.is_a?(Proc) ? block : nil
      end

      def call(args : Array(Type), env : Environment)
        if @block
          @block.not_nil!.call(args, env, @arg_type)
        elsif @arg_type == :splat
          args[0]
        else
          args
        end.as(Type)
      end
    end
  end
end
