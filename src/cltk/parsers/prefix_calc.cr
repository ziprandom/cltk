# Description:	This file contains a parser for a simple prefix calculator.

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

    # A parser for a simple prefix calculator.
    class PrefixCalc < Parser
      production(:e) do
	clause("NUM") { |n| n[0] }

	clause("PLS e e") { |e| (e[1] as Int32) + (e[2] as Int32) }
	clause("SUB e e") { |e| (e[1] as Int32) - (e[2] as Int32) }
	clause("MUL e e") { |e| (e[1] as Int32) * (e[2] as Int32) }
	clause("DIV e e") { |e| (e[1] as Int32) / (e[2] as Int32) }
        nil
      end

      finalize
    end
  end
end
