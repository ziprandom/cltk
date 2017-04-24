# Description:	This file contains a lexer for a simple calculator.

############
# Requires #
############

#######################
# Classes and Modules #
#######################

require "../lexer.cr"
module CLTK

  # The CLTK::Lexers module contains the lexers that are included as part of
  # the CLTK project.
  module Lexers

    # The Calculator lexer is a simple lexer for use with several of the
    # provided parsers.
    class Calculator < Scanner
      extend CLTK::Scanner::LexerCompatibility

      #################
      # Default State #
      #################

      rule("+")	        { { :PLS } }
      rule("-")         { { :SUB } }
      rule("*")	        { { :MUL } }
      rule("/")	        { { :DIV } }

      rule("(")	        { { :LPAREN } }
      rule(")")	        { { :RPAREN } }

      rule(/[0-9]+/)	{ |t| {:NUM, t.to_i32}}

      rule(" ")
    end
  end
end
