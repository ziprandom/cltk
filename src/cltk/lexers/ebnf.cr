# coding: utf-8
# Description:	This file contains a lexer for Extended Backus–Naur Form.

############
# Requires #
############

# Crystal Language Toolkit
require "../scanner"
require "../token"

#######################
# Classes and Modules #
#######################

module CLTK
  module Lexers

    # The EBNF lexer is used by the CLTK::CFG class.
    class EBNF < CLTK::Scanner
      extend CLTK::Scanner::LexerCompatibility
      #################
      # Default State #
      #################

      rule("*") { { :STAR     } }
      rule("+") { { :PLUS     } }
      rule("?") { { :QUESTION } }
      rule(".") { { :DOT      } }

      rule(/[a-z0-9_']+/) { |t| {:NONTERM, t.to_s} }
      rule(/[A-Z0-9_']+/) { |t| {:TERM,    t.to_s} }

      rule(" ")

    end
  end
end
