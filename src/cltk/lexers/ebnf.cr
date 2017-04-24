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

      def self.lex(string)
        super(string).tokens.map do |token|
          # pos = CLTK::StreamPosition.new(position[:position], 0, position[:position], position[:size])
	  CLTK::Token.new(token[0], token[1]?)
        end << CLTK::Token.new(:EOS)
      end

      def lex(string)
        self.class.lex(string)
      end

    end
  end
end
