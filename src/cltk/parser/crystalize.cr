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

class CrystalizeVisitor

  @@productions = Array(CLTK::CFG::Production).new

  def self.to_class(klass)
    %{
      class #{klass} < CLTK::Parser
        @@symbols = #{visit klass.symbols}
        @@env = #{visit klass.env}
        @@lh_sides = #{visit klass.lh_sides}
        @@states = #{visit klass.states}
        @@procs = #{visit klass.procs}
        @@token_hooks = #{visit klass.token_hooks, "String, Array(Proc(Environment, Nil))"}
        @@productions = #{visit_productions @@productions }
      end
    }
  end

  def self.visit(parser : CLTK::Parser)
    %{CLTK::Parser.new(
        #{visit parser.symbols},
        #{visit parser.lh_sides},
        #{visit parser.states},
        #{visit parser.procs},
        #{visit parser.token_hooks, "String, Array(Proc(Environment, Nil))"},
        #{visit parser.env}
      )}
  end
  def self.visit_productions(productions)
    if productions.empty?
      return "[] of CFG::Production"
    else
      productions_crystalized = productions.map do |production|
        %{CLTK::CFG::Production.new(
          #{visit production.id},
          #{visit production.lhs},
          #{visit production.rhs}
        )}
      end.join(", ")
      "[#{productions_crystalized}]"
    end
  end
  def self.visit(state : CLTK::Parser::State)
    %{CLTK::Parser::State.new(
        #{visit(state.id)},
        #{visit(state.actions)},
        (#{visit(state.items)}).as(Array(CLTK::CFG::Item)),
      )}
  end

  def self.visit(action : CLTK::Parser::Accept)
    %{CLTK::Parser::Accept.new.as(CLTK::Parser::Action)}
  end

  def self.visit(action : CLTK::Parser::Reduce)
    %{CLTK::Parser::Reduce.new(@@productions[#{visit action.production}]).as(CLTK::Parser::Action)}
  end

  def self.visit(action : CLTK::Parser::GoTo)
    %{CLTK::Parser::GoTo.new(#{visit action.id}).as(CLTK::Parser::Action)}
  end

  def self.visit(action : CLTK::Parser::Shift)
    %{CLTK::Parser::Shift.new(#{visit action.id}).as(CLTK::Parser::Action)}
  end

  def self.visit(item : CLTK::CFG::Item)
    %{CLTK::CFG::Item.new(
      #{visit item.dot},
      #{visit item.id},
      #{visit item.lhs},
      #{visit item.rhs},
    )}
  end

  def self.visit(production : CLTK::CFG::Production)
    index = @@productions.index(production)
    if index
      "#{index}"
    else
      @@productions << production
      "#{@@productions.size()-1}"
    end
  end

  def self.visit(node : {CLTK::Parser::ProdProc, Int32})
    %{{
        #{visit node[0]},
        #{visit node[1]}
      }}
  end

  def self.visit(node : CLTK::Parser::ProdProc)
    %{CLTK::Parser::ProdProc.new(
        #{visit node.arg_type},
        #{visit node.selections}
      ) #{node.crystalized_block}}
  end

  def self.visit(token_hook : Proc(CLTK::Parser::Environment, Nil))
    %{
      Proc(CLTK::Parser::Environment, Nil).new { nil }
    }
  end

  def self.visit(env : (CLTK::Parser::Environment.class)?)
    "#{env}"
  end

  def self.visit(node : Array, klass = nil)
    if node.empty?
      "#{node.class.to_s}.new"
    else
      "[" + node.map{ |e| visit(e) }.join(", ") + "]"
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
      "#{node.class.to_s}.new"
    else
      "{" +
        node.select{|_, v| v.size > 0}.map do |key, value|
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

module CLTK
  class Parser
    # make these class attributes accessible from outside
    # (for the Visitor to work on them)
    class_getter :symbols, :states, :procs, :lh_sides, :token_hooks, :env

    def self.crystalize()
      puts CrystalizeVisitor.to_class(self);
    end
  end
end
