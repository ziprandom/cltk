module CLTK
  class Parser
    # make these class attributes accessible from outside
    # (for the Visitor to work on them)
    class_getter :symbols, :states, :procs, :lh_sides, :token_hooks, :env

    def self.tupelize(name : Symbol)
      tupelize(name.to_s)
    end

    def self.tupelize(name : String = {{@type.stringify}})
      puts %{
        module #{name}
          class Environment < CLTK::Parser::Environment; end
          Parser = #{TupelizeVisitor.to_tuple(self)}

          def self.new
            unless Parser[:symbols].any?
              raise CLTK::UselessParserException.new
            end
            CLTK::CLTKParser(self).new
          end

          def self.parse(tokens, options = NamedTuple.new)
            CLTK::CLTKParser(self).new.parse(tokens, options)
          end
        end
      }
    end
  end
end

module CLTK
  class Parser
    struct ProdProc
      property :crystalized_block
      @crystalized_block : String? = nil

      # Keep Track of the String representation
      # of this Procs Code Block for later
      # serialization
      macro new(*args, &block)
        instance = {{@type}}.allocate
        {% if args.empty? %}
          instance.initialize {% if block %}{{block}}{% end%}
        {% else %}
        instance.initialize(*{{args}}) {% if block %}{{block}}{% end %}
        {% end %}
        instance.crystalized_block = {% if block %}"Proc(CLTK::Type, Environment, Symbol, CLTK::Type).new {{block}}" {% else %}nil{% end %}
        instance
      end
    end
  end
end

class TupelizeVisitor

  def self.define_types(klass)
    %{
      class #{klass}
        class Environment < #{klass.env}; end
      end
      alias Environment = #{klass}::Environment;
    }
  end
  def self.to_tuple(klass)
    %{
      NamedTuple.new(
        symbols: #{visit_symbols klass.symbols},
          lh_sides: #{visit_lh_sides klass.lh_sides},
          states: #{visit_states klass.states},
          procs: #{visit_procs klass.procs},
          token_hooks: #{visit klass.token_hooks, "String, Array(Proc(Environment, Nil))"},
          env: #{visit klass.env}
        )
    }
  end

  def self.visit_procs(procs)
    %{
      #{visit procs.values}
    }
  end

  def self.visit_actions(actions)
    actions.map do |symbol, actions|
      "\"#{symbol}\": " + visit(actions)
    end.compact.join(", ")
  end

  def self.visit_state(state)
    %{NamedTuple.new(
        actions: NamedTuple.new(
          #{visit_actions(state.actions)}
        ),
        items: #{visit state.items}
    )}
  end

  def self.visit_states(states)
    %{ Tuple.new(#{states.map { |state| visit_state(state) }.join(", ")}) }
  end

  def self.visit_lh_sides(lh_sides)
    %{ Tuple.new(#{lh_sides.values.map { |v| ":\"#{v}\"" }.join(", ")}) }
  end

  def self.visit_symbols(symbols)
    %{ Tuple.new(#{symbols.map { |symbol| ":#{symbol}" }.join(", ")}) }
  end

  def self.visit(action : CLTK::Parser::Accept)
    %{ NamedTuple.new(id: 0, type: :ACCEPT) }
  end

  def self.visit(action : CLTK::Parser::Reduce)
    %{ NamedTuple.new(id: #{action.production.id}, type: :REDUCE, production: #{visit action.production}) }
  end

  def self.visit(action : CLTK::Parser::GoTo)
    %{ NamedTuple.new(id: #{action.id}, type: :GOTO) }
  end

  def self.visit(action : CLTK::Parser::Shift)
    %{ NamedTuple.new(id: #{action.id}, type: :SHIFT) }
  end

  def self.visit(item : CLTK::CFG::Item)
    %{NamedTuple.new(id: #{visit item.id}, dot: #{visit item.dot}, lhs: #{visit item.lhs}, rhs: #{visit item.rhs}) }
  end

  def self.visit(production : CLTK::CFG::Production)
    rhs = if production.rhs.size > 0
            production.rhs.map { |s| visit s}
          else
            "Array(String).new"
          end
    %{ NamedTuple.new(id: #{visit production.id}, lhs: :#{production.lhs}, rhs: #{rhs}) }
  end

  def self.visit(node : {CLTK::Parser::ProdProc, Int32})
    %{
      Tuple.new(#{visit node[0]}, #{visit node[1]})
    }
  end

  def self.visit(node : CLTK::Parser::ProdProc)
    procpart = node.crystalized_block ?
                 "proc: #{node.crystalized_block}" : "proc: nil"
    %{
      NamedTuple.new(
        arg_type: #{visit node.arg_type},
        selections: #{visit node.selections},
        #{procpart}
      )
    }
  end

  def self.visit(token_hook : Proc(CLTK::Parser::Environment, Nil))
    %{
      Proc(CLTK::Parser::Environment, Nil).new { nil }
    }
  end

  def self.visit(env : (CLTK::Parser::Environment.class)?)
    "#{env}"
  end

  def self.visit(node : Array)
    if node.empty?
      "Tuple.new"
    else
      "Tuple.new(" + node.map{ |e| visit(e) }.join(", ") + ")"
    end
  end

  def self.visit(node : String)
    %{"#{node}"}
  end

  def self.visit(node : Int)
    "#{node}"
  end

  def self.visit(node : Symbol)
    ":#{node}"
  end

  def self.visit(node : Nil)
    "nil"
  end

  def self.visit(node : Hash, klass = nil)
    if node.empty?
      "NamedTuple.new"
    else
      "{" +
        node.map do |key, value|
        "#{visit(key)} => #{visit(value)}"
      end.join(",\n") +
        "\n}"
    end
  end
      end

macro class_getter(*attributes)
  {% for attribute in attributes %}
    def self.{{attribute.id}}
      @@{{attribute.id}}
    end
  {% end %}
end
