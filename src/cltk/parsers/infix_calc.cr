# Description:	This file contains a parser for a simple infix calculator.

############
# Requires #
############

# Crystal Language Toolkit
require "../parser"
#######################
# Classes and Modules #
#######################

module CLTK

  # The CLTK::Parsers module contains the parsers that are included as part
  # of the CLTK project.
  module Parsers

    # A parser for a simple infix calculator.
    class InfixCalc < Parser

      left :PLS, :SUB
      right :MUL, :DIV

      production(:e) do
	clause("NUM") { |n| n.as(Int32) }

	clause("LPAREN e RPAREN") { |rparen, a, lparen | a }

	clause("e PLS e") { |e0, op, e1| e0.as(Int32) + e1.as(Int32) }
	clause("e SUB e") { |e0, op, e1| e0.as(Int32) - e1.as(Int32) }
	clause("e MUL e") { |e0, op, e1| e0.as(Int32) * e1.as(Int32) }
	clause("e DIV e") { |e0, op, e1| e0.as(Int32) // e1.as(Int32) }
      end

      finalize
    end
  end
end
