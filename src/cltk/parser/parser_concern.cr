require "./state"

require "../cfg"
require "./state"
require "./actions"
require "./exceptions/*"
require "./prod_proc"
{% if env("VERBOSE") == "procs" %}require "./prod_proc_debug"{% end %}
  require "./type"
require "./parse_stack"
require "./parse_macro"
require "./environment"

module CLTK
  class Parser
    # A module that adds the parse algorithm into the class or module its included in.
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
              raise CLTK::Parser::Exceptions::UselessParserException.new
            end
          end

          def parse(tokens, opts : NamedTuple? = nil)
            {{@type}}.parse(tokens, opts ? opts.merge(@opts) : @opts)
          end
        end

      end
    end
  end
end
