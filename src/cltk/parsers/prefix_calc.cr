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
	clause("NUM") { |n| n }

	clause("PLS e e") { |op, e0, e1| e0.as(Int32) + e1.as(Int32) }
	clause("SUB e e") { |op, e0, e1| e0.as(Int32) - e1.as(Int32) }
	clause("MUL e e") { |op, e0, e1| e0.as(Int32) * e1.as(Int32) }
	clause("DIV e e") { |op, e0, e1| e0.as(Int32) // e1.as(Int32) }
        nil
      end

      finalize
    end
  end
end
