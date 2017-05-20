require "msgpack"
require "./prod_proc"
require "./parse_macro"

module CLTK
  abstract class Parser
    macro inherited
      def self.to_parser
        StandaloneParser.new(
          @@lh_sides, @@symbols, @@states,
          #@@token_hooks
        )
      end
    end
    class StandaloneParser
      def_parse(:instance)
      getter ::lh_sides, :symbols, :states
      MessagePack.mapping({
                            lh_sides:  Hash(Int32, String),
                            symbols: Array(String),
                            states: Array(State)
                          })
      def initialize(
            @lh_sides : Hash(Int32, String),
            @symbols : Array(String),
            @states : Array(State),
            #@token_hooks : Hash(String, Array(Proc(Environment, Nil)))
          )
      end
    end

    class State
      MessagePack.mapping({
                            id: Int32,
                            actions: Hash(String, Array(CLTK::Parser::Action)),
                            items: Array(CFG::Item)
                          })
    end


    abstract struct Action
      def self.new(packer : MessagePack::Unpacker)
        self.from_msgpack(packer)
      end

      def self.from_msgpack(pull : MessagePack::Unpacker)
        type = pull.read
        case type
        when "goto"
          CLTK::Parser::Actions::GoTo.new(pull)
        when "accept"
          CLTK::Parser::Actions::Accept.new(pull)
        when "shift"
          CLTK::Parser::Actions::Shift.new(pull)
        when "reduce"
          CLTK::Parser::Actions::Reduce.new(pull)
        else
          raise "this is not an action"
        end
      end

      def to_msgpack(packer : MessagePack::Packer); end

  end

    module Actions
      struct Accept
        MessagePack.mapping({id: Int32})
        def to_msgpack(packer : MessagePack::Packer )
          "accept".to_msgpack(packer)
          previous_def(packer)
        end
      end
      struct GoTo
        MessagePack.mapping({id: Int32})
        def to_msgpack(packer : MessagePack::Packer )
          "goto".to_msgpack(packer)
          previous_def(packer)
        end
      end
      struct Reduce
        MessagePack.mapping({id: Int32, production: CLTK::CFG::Production})
        def to_msgpack(packer : MessagePack::Packer )
          "reduce".to_msgpack(packer)
          previous_def(packer)
        end
      end
      struct Shift
        MessagePack.mapping({id: Int32})
        def to_msgpack(packer : MessagePack::Packer )
          "shift".to_msgpack(packer)
          previous_def(packer)
        end
      end
    end
  end

  class CFG
    class Production
      MessagePack.mapping({id: Int32, lhs: String, rhs: Array(String)})
    end
    class Item
      MessagePack.mapping({id: Int32, dot: Int32, lhs: String, rhs: Array(String)})
    end
  end
end
