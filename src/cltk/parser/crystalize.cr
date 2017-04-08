require "./prod_proc_debug"
module CLTK
  class Parser

    macro class_getter(*attributes)
      {% for attribute in attributes %}
        def self.{{attribute.id}}
          @@{{attribute.id}}
        end
      {% end %}
    end

    # make these class attributes accessible from outside
    # (for the Visitor to work on them)
    class_getter :symbols, :states, :procs, :lh_sides, :token_hooks, :env

    def self.crystalize(name : (Symbol|String) = self.name)
      puts CrystalizeVisitor.to_module(self, name.to_s);
    end

    # A Visitor to turn a finalized Parser into its crystal source code representation
    # to be included in a project. This way the Parser can be used at runtime without
    # having to be finalized upon instantiation.
    module CrystalizeVisitor

      @@productions = Array(CLTK::CFG::Production).new

      # create the crystal source code representation of the given Parser Class
      # as a module with the given name
      def self.to_module(klass : CLTK::Parser.class, name : String)
        %{
          module #{name}
            class Environment < CLTK::Parser::Environment; end
            include CLTK::Parser::ParserConcern
            SYMBOLS = #{visit klass.symbols}
            ENV = #{visit klass.env}
            LH_SIDES = #{visit klass.lh_sides}
            STATES = #{visit klass.states}
            PROCS = #{visit klass.procs.values}
            TOKEN_HOOKS = #{visit klass.token_hooks, "String, Array(Proc(Environment, Nil))"}
            PRODUCTIONS = #{visit_productions @@productions }
          end
        }
      end

      # create the crystal source code representation of the given Parser Class
      # as a class
      def self.to_class(klass : CLTK::Parser.class)
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

      private def self.visit(parser : CLTK::Parser)
        %{CLTK::Parser.new(
            #{visit parser.symbols},
            #{visit parser.lh_sides},
            #{visit parser.states},
            #{visit parser.procs},
            #{visit parser.token_hooks, "String, Array(Proc(Environment, Nil))"},
            #{visit parser.env}
          )}
      end

      private def self.visit_productions(productions)
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
      private def self.visit(state : CLTK::Parser::State)
        %{CLTK::Parser::State.new(
            #{visit(state.id)},
            #{visit(state.actions)},
            (#{visit(state.items)}).as(Array(CLTK::CFG::Item)),
          )}
      end

      private def self.visit(action : CLTK::Parser::Actions::Accept)
        %{CLTK::Parser::Actions::Accept.new.as(CLTK::Parser::Action)}
      end

      private def self.visit(action : CLTK::Parser::Actions::Reduce)
        %{CLTK::Parser::Actions::Reduce.new(PRODUCTIONS[#{visit action.production}]).as(CLTK::Parser::Action)}
      end

      private def self.visit(action : CLTK::Parser::Actions::GoTo)
        %{CLTK::Parser::Actions::GoTo.new(#{visit action.id}).as(CLTK::Parser::Action)}
      end

      private def self.visit(action : CLTK::Parser::Actions::Shift)
        %{CLTK::Parser::Actions::Shift.new(#{visit action.id}).as(CLTK::Parser::Action)}
      end

      private def self.visit(item : CLTK::CFG::Item)
        %{CLTK::CFG::Item.new(
          #{visit item.dot},
          #{visit item.id},
          #{visit item.lhs},
          #{visit item.rhs},
        )}
      end

      private def self.visit(production : CLTK::CFG::Production)
        index = @@productions.index(production)
        if index
          "#{index}"
        else
          @@productions << production
          "#{@@productions.size()-1}"
        end
      end

      private def self.visit(node : {CLTK::Parser::ProdProc, Int32})
        %{{
            #{visit node[0]},
            #{visit node[1]}
          }}
      end

      private def self.visit(node : CLTK::Parser::ProdProc)
        %{CLTK::Parser::ProdProc.new(
            #{visit node.arg_type},
            #{visit node.selections}
          ) #{node.crystalized_block}}
      end

      private def self.visit(token_hook : Proc(CLTK::Parser::Environment, Nil))
        %{
          Proc(CLTK::Parser::Environment, Nil).new { nil }
        }
      end

      private def self.visit(env : (CLTK::Parser::Environment.class)?)
        "#{env}"
      end

      private def self.visit(node : Array, klass = nil)
        if node.empty?
          "#{node.class.to_s}.new"
        else
          "[" + node.map{ |e| visit(e) }.join(", ") + "]"
        end
      end

      private def self.visit(node : String)
        %{"#{node}"}
      end

      private def self.visit(node : Int)
        "#{node}"
      end

      private def self.visit(node : Symbol)
        ":#{node}"
      end

      private def self.visit(node : Nil)
        "nil"
      end

      private def self.visit(node : Hash, klass = nil)
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
  end
end
