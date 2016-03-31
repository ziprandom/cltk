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
	clause("NUM") { |n| n[0] as Int32 }

	clause("LPAREN e RPAREN") { |a| a[1] }

	clause("e PLS e") { |a| (a[0] as Int32) + (a[2] as Int32) }
	clause("e SUB e") { |a| (a[0] as Int32) - (a[2] as Int32) }
	clause("e MUL e") { |a| (a[0] as Int32) * (a[2] as Int32) }
	clause("e DIV e") { |a| (a[0] as Int32) / (a[2] as Int32) }
        nil
      end

      finalize
    end
  end
end
