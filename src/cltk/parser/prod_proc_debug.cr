module CLTK
  class Parser
    struct ProdProc
      property :crystalized_block
      @crystalized_block : String?

      # Keep Track of the String representation
      # of this Procs Code Block for later
      # serialization
      macro new(*args, &block)
        instance = {{@type}}.allocate
        {% if args.empty? %}
          instance.initialize {{block}}
        {% else %}
          instance.initialize(*{{args}}) {{block}}
        {% end %}
        instance.crystalized_block = {{block.stringify}}
        instance
      end
    end
  end
end
