module CLTK
  module Visitor

    def yield_with_env(item, &block)
      with @env yield(item)
    end

    macro on(klass, &block)
      def wrapped_visit({{block.args.first}} : {{klass.id}})
        yield_with_env({{block.args.first}}) do | {{block.args.first}} |
          {{block.body}}
        end
      end
    end

    def wrapped_visit(item)
      raise "no patterns defined for #{item} (#{item.class})"
    end

    def visit(node)
      wrapped_visit(node)
    end
  end
end
