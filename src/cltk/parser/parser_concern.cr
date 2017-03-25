require "./state"
require "../named_tuple_extensions"

require "../cfg"
require "./state"
require "./actions"
require "./exceptions/*"
require "./prod_proc"
{% if env("VERBOSE") == "procs" %}require "./prod_proc_debug"{% end %}
require "./parse_stack"
require "./parse_macro"
require "./environment"

module CLTK
  module ParserConcern

    macro included

      def self.new
        Parser.new
      end

      def_parse

      class Parser
        @opts = {
          env: Environment.new
        }

        def initialize
          unless SYMBOLS.any?
            raise CLTK::UselessParserException.new
          end
        end

        def parse(tokens, opts : NamedTuple? = nil)
          {{@type}}.parse(tokens, opts ? opts.merge(@opts) : @opts)
        end
      end

    end
  end
end
