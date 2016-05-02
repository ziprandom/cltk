# coding: utf-8
# Author:	Chris Wailes <chris.wailes@gmail.com>
# Project: 	Ruby Language Toolkit
# Date:		2011/05/09
# Description:	This file defines a simple parser for the Kazoo language.

# RLTK Files
require "../../../src/cltk/parser"

# Tutorial Files
require "./kast"

module Kazoo
  class Parser < CLTK::Parser

    left :IN
    left :ASSIGN, :SEQ
    left :LT, :GT, :PIPE, :AMP, :EQL, :BANG, :NEG, :ELSE
    left :PLUS, :SUB
    left :MUL, :DIV

    production(:input, "statement SEMI") { |s, _| s }

    production(:statement) do
      clause("e")		         { |e| e }
      clause("e_list")		         { |e| e }
      clause("ex")		         { |e| e }
      clause("p")		         { |p| p }
      clause("f")		         { |f| f }
    end

    production(:e_list) do
      clause("e SEMI e")                 { |e1, _, e2| ExpressionList.new( [e1, e2] ) }
      clause("e_list SEMI e")            { |e1, _, e2| ExpressionList.new( ((e1 as ExpressionList).expressions as Array) + [e2] ) }

    end

    production(:e) do

      clause("LPAREN e RPAREN")          { |_, e, _| e                 }
      clause("COMMENT")                  { nil }

      clause("NUMBER"  )                 { |n|         Kazoo::ANumber.new(n)   }
      clause("IDENT"   )                 { |i|         Variable.new(i) }
      clause("e PLUS e")                 { |e0, _, e1| Add.new(e0, e1) }
      clause("e SUB e" )                 { |e0, _, e1| Sub.new(e0, e1) }
      clause("e MUL e" )                 { |e0, _, e1| Mul.new(e0, e1) }
      clause("e DIV e" )                 { |e0, _, e1| Div.new(e0, e1) }
      clause("e LT e"  )                 { |e0, _, e1| LT.new(e0, e1)  }

      clause("IDENT ASSIGN e")	         { |e0, _, e1| Assign.new(e0, e1) }
      clause("e GT e"        )	         { |e0, _, e1|     GT.new(e0, e1) }
      clause("e EQL e"       )	         { |e0, _, e1|    Eql.new(e0, e1) }
      clause("e PIPE e"      )	         { |e0, _, e1|     Or.new(e0, e1) }
      clause("e AMP e"       )	         { |e0, _, e1|    And.new(e0, e1) }
      clause("e SEQ e"       )	         { |e0, _, e1| Binary.new(e0, e1) }

      clause("SUB e", "NEG")	         { |_, e| Sub.new(e, Kazoo::ANumber.new(0.0)) }
      clause("BANG e")		         { |_, e| If.new(e, Kazoo::ANumber.new(0.0), Kazoo::ANumber.new(1.0)) }

      clause("IDENT LPAREN args RPAREN") { |i, _, args, _| Call.new(i, args) }

      clause("FOR IDENT ASSIGN e COMMA e COMMA e IN e END") do |_, i, _, e0, _, e1, _, e2, _, e3|
	For.new(i, e0, e1, e2, e3)
      end
      clause("IF e THEN e ELSE e")       { |_, e0, _, e1, _, e2| If.new(e0, e1, e2) }
      clause(:ex) { |e| e }
      clause(:f) { |e| e }
    end

    list(:args, :e, :COMMA)

    production(:ex, "EXTERN p_body")	 { |_, p| p }
    production(:p, "DEF p_body")	 { |_, p| p }
    production(:f, "p e END")		 { |p, e| Function.new(p, e) }

    production(:p_body, "IDENT LPAREN arg_defs RPAREN") do |name, _, arg_names, _|
      x = Prototype.new(name, (arg_names as Array).map {|a| a as String})
    end

    list(:arg_defs, :IDENT, :COMMA)

    finalize
  end
end
