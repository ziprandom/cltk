# coding: utf-8
# Author:	Chris Wailes <chris.wailes@gmail.com>
# Project: 	Ruby Language Toolkit
# Date:		2011/05/09
# Description:	This file defines a simple parser for the Kazoo language.

# RLTK Files
require "./ktype"
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
      clause("e SEMI e")                 { |e1, _, e2| ExpressionList.new( expressions: [e1.as(Expression), e2.as(Expression)] ) }
      clause("e_list SEMI e")            { |e1, _, e2| ExpressionList.new( expressions: e1.as(ExpressionList).expressions + [e2.as(Expression)]) }

    end

    production(:e) do

      clause("LPAREN e RPAREN")          { |_, e, _| e                 }
      clause("COMMENT")                  { nil }

      clause("NUMBER"  )                 { |n|         Kazoo::ANumber.new(value: n.as(Float64))   }
      clause("IDENT"   )                 { |i|         Variable.new(name: i.as(String)) }
      clause("e PLUS e")                 { |e0, _, e1| Add.new(left: e0.as(Expression), right: e1.as(Expression)) }
      clause("e SUB e" )                 { |e0, _, e1| Sub.new(left: e0.as(Expression), right: e1.as(Expression)) }
      clause("e MUL e" )                 { |e0, _, e1| Mul.new(left: e0.as(Expression), right: e1.as(Expression)) }
      clause("e DIV e" )                 { |e0, _, e1| Div.new(left: e0.as(Expression), right: e1.as(Expression)) }
      clause("e LT e"  )                 { |e0, _, e1| LT.new(left: e0.as(Expression), right: e1.as(Expression))  }

      clause("IDENT ASSIGN e")	         { |e0, _, e1| Assign.new(name: e0.as(String), right: e1.as(Expression)) }
      clause("e GT e"        )	         { |e0, _, e1|     GT.new(left: e0.as(Expression), right: e1.as(Expression)) }
      clause("e EQL e"       )	         { |e0, _, e1|    Eql.new(left: e0.as(Expression), right: e1.as(Expression)) }
      clause("e PIPE e"      )	         { |e0, _, e1|     Or.new(left: e0.as(Expression), right: e1.as(Expression)) }
      clause("e AMP e"       )	         { |e0, _, e1|    And.new(left: e0.as(Expression), right: e1.as(Expression)) }
      clause("e SEQ e"       )	         { |e0, _, e1| Binary.new(left: e0.as(Expression), right: e1.as(Expression)) }

      clause("SUB e", "NEG")	         { |_, e| Sub.new(right: e.as(Expression), left: Kazoo::ANumber.new(value: 0.0)) }
      clause("BANG e")		         { |_, e| If.new(cond: e.as(Expression), thenExp: Kazoo::ANumber.new(value: 0.0), elseExp: Kazoo::ANumber.new(value: 1.0)) }

      clause("IDENT LPAREN args RPAREN") { |i, _, args, _| Call.new(name: i.as(String), args: args.as(Array).map(&.as(Expression))) }

      clause("FOR IDENT ASSIGN e COMMA e COMMA e IN e END") do |_, i, _, e0, _, e1, _, e2, _, e3|
	For.new(var: i.as(String), init: e0.as(Expression), cond: e1.as(Expression), step: e2.as(Expression), body: e3.as(Expression))
      end
      clause("IF e THEN e ELSE e")       { |_, e0, _, e1, _, e2| If.new(cond: e0.as(Expression), thenExp: e1.as(Expression), elseExp: e2.as(Expression)) }
      clause(:ex) { |e| e.as(Expression) }
      clause(:f) { |e| e.as(Expression) }
    end

    list(:args, :e, :COMMA)

    production(:ex, "EXTERN p_body")	 { |_, p| p }
    production(:p, "DEF p_body")	 { |_, p| p }
    production(:f, "p e END")		 { |p, e| Function.new(proto: p.as(Kazoo::Prototype), body: e.as(Expression)) }

    production(:p_body, "IDENT LPAREN arg_defs RPAREN") do |name, _, arg_names, _|
      x = Prototype.new(name: name.as(String), arg_names: arg_names.as(Array).map {|a| a.as(String)})
    end

    list(:arg_defs, :IDENT, :COMMA)

    finalize(use: "./kazoo_parser.bin")
  end
end
