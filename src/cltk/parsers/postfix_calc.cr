# Description:	This file contains a parser for a simple postfix calculator.

############
# Requires #
############

# Crystal Language Toolkit
require "../parser"

#######################
# Classes and Modules #
#######################

module CLTK
  module Parsers

    # A parser for a simple post-fix calculator.
    class PostfixCalc < Parser
      production(:e) do
	clause("NUM") { |n| n[0] as Int32}

	clause("e e PLS") { |e| (e[0] as Int32) + (e[1] as Int32) }
	clause("e e SUB") { |e| (e[0] as Int32) - (e[1] as Int32) }
	clause("e e MUL") { |e| (e[0] as Int32) * (e[1] as Int32) }
	clause("e e DIV") { |e| (e[0] as Int32) / (e[1] as Int32) }
        nil
      end

      finalize
    end
  end
end
