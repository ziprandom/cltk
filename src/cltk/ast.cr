module CLTK
  abstract class ASTNode
    macro make_value_methods
      macro self.value(**args)
        \{%
          args.keys.map do |k|
            if args[k].is_a? Assign
              VALUES << { k, args[k].target, args[k].value }
            else
              VALUES << { k, args[k] }
            end
          end
        %}

        \{{
          args.keys.map do |k|
            "accessors #{k}, " + (args[k].is_a?(Assign) ? "#{args[k].target}, #{args[k].value}" : "#{args[k]}")
          end.join("\n")
         }}
      end

      macro accessors(name, type, default)
        def \\{{name}}
          @\\{{name}}
        end

        def \\{{name}}=(@\\{{name}} : \\{{type}}); end
      end

      macro values(args)
        \{%
          args.keys.map do |k|
            VALUES << { k, args[k] }
          end
        %}
        property \{{args.keys.map{|k| "#{k} : #{args[k]}"}.join(", ").id}}
      end

      macro inherited
        make_value_methods
      end
    end

    def self.values
      NamedTuple.new()
    end

    def values
      NamedTuple.new()
    end

    def ==(other)
      true
    end

    def inspect
      "#{self.class.name}(" +
        if vs = values
          vs.map do |k, v|
            value = if v.is_a?(Array)
                      "[" + v.map{ |vv| vv.inspect.as(String) }.join(", ") + "]"
                    elsif v.is_a?(Hash)
                      "{" + v.map{ |kk, vv| "#{kk.inspect}: #{vv.inspect}".as(String) }.join(", ") + "}"
                    else
                      v.inspect
                    end
            "#{k}: #{value}"
          end.join(", ")
        else
          ""
        end + ")"
    end

    def_clone

    macro inherited

      make_value_methods

      macro finished
        def_clone

        def ==(other : \{{@type}})
          \{{ (VALUES.map { |v| "(@#{v[0].id} == other.#{v[0].id})".id } + ["super(other)".id]).join(" && ").id }}
        end

        def self.values
          super.merge NamedTuple.new(\{{ VALUES.map { |v| "#{v[0].id}: #{v[1].id}".id }.join(",").id }})
        end

        def values
          super.merge NamedTuple.new(\{{ VALUES.map { |v| "#{v[0].id}: @#{v[0]}".id }.join(",").id }})
        end

        \{%
           signatures = VALUES.map { |v| "@#{v[0].id} : #{v[1]}" + (v[2] ? "= #{v[2]}" : "") }
           signature = (signatures + ["**rest"]).join(", ").id
          %}

        def initialize(\{{signature}})
          super(**rest)
        end

      end
    end

    VALUES = [] of Tuple(Symbol, Object.class)

    macro inherited
      VALUES = [] of Tuple(Symbol, Object.class)
    end

    make_value_methods

  end
end
