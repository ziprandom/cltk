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
	clause("NUM") { |n| n.as(Int32)}

	clause("e e PLS") { |e0, e1, op| e0.as(Int32) + e1.as(Int32) }
	clause("e e SUB") { |e0, e1, op| e0.as(Int32) - e1.as(Int32) }
	clause("e e MUL") { |e0, e1, op| e0.as(Int32) * e1.as(Int32) }
	clause("e e DIV") { |e0, e1, op| e0.as(Int32) // e1.as(Int32) }
        nil
      end

      finalize
    end
  end
end
