# Author:		Chris Wailes <chris.wailes@gmail.com>
# Project: 	Ruby Language Toolkit
# Date:		2011/05/09
# Description:	This file defines a simple parser for the Kazoo language.

# RLTK Files
require "../src/cltk/parser"
require "./kast"

module Kazoo
  class Parser < CLTK::Parser

    left :ASSIGN
    left :FUN
    left :LPAREN, :LBRACK, :LCBRACK
    right :RPAREN, :RBRACK, :RCBRACK
    left :PLUS, :SUB
    left :MUL, :DIV


    production(:input, "statement") { |s| s }

    production(:sep) do
      clause("SEMI") { |n| n }
      clause("CR") { |n| n }
      nil
    end
    production(:commax) do
      clause("CR* COMMA CR*") {|e0, e1| e0 }
    end
    production(:hash_pair) do
      clause("IDENT COLON e") {|e0, x, e1| [e0, e1]}
    end
    build_list_production(:hash_pairs, :hash_pair, :commax)
    build_list_production(:array_elements, :e, :commax)

    production(:e) do
      clause("LPAREN e RPAREN") { |e0, e1| e1 }
      clause("NUMBER")	        { |n| ANumber.new(n as Float64) }
      clause("IDENT")	        { |i| Variable.new(i); }
      clause("NIL")	        { KNil.new }
      clause("TRUE")            { KTrue.new }
      clause("FALSE")           { KFalse.new }
      clause("e OR e")          { |e0, o, e2| KOr.new(e0, e2) as Expression}
      clause("e AND e")         { |e0, a, e2| KAnd.new(e0, e2)}
      clause("IDENT")	        { |i| Variable.new(i); }
      clause("e LPAREN array_elements RPAREN") do |e lp arguments rp|
        parameters = (arguments as Array).reduce([] of Expression) do |all, e|
          all + [e as Expression]
        end
        FunCall.new(e, parameters)
      end

      clause("STRING")	{ |s| AString.new(s) }
      clause("e PLUS e")	{ |e0, p, e1| a = Add.new(e0, e1); a }
      clause("e SUB e")	{ |e0, p, e1| Sub.new(e0, e1) }
      clause("e MUL e")	{ |e0, p, e1| Mul.new(e0, e1) }
      clause("e DIV e")	{ |e0, p, e1| Div.new(e0, e1) }
      clause("LCBRACK hash_pairs RCBRACK") do |lb hp rb|
        hash = Hash(Variable, Expression).new
        pairs = (hp as Array).reduce([] of Expression) do |all, e |
          all + [[Variable.new((e as Array)[0]), (e as Array)[1]]]
        end
        pairs.each do |pair|
          key = (pair as Array)[0]
          value = (pair as Array)[1]
          if value.is_a? Expression
            hash[key as Variable] = (value as Expression)
          end
        end
        AHash.new(hash as Hash(Variable, Expression))
      end
      clause("LBRACK array_elements RBRACK") do |lb, ae, rb|
        s = (ae as Array).reduce([] of Expression) do |all, e |
          all + [e as Expression]
        end
        a = AArray.new(s)
      end

      clause("fun_def") { |e| e }
      clause("varassign") { |e| e }

      nil
    end

    production(:varassign) do
      clause("IDENT ASSIGN e") do |ident, assign, e|
        VarAssign.new(Variable.new(ident), e)
      end
    end

    build_list_production("args", "IDENT", :commax)

    production(:fun_head) do
      clause("DEF IDENT LPAREN args RPAREN") do |df, ident, lp, args, rp|
        [ident, args]
      end
      clause("FUN LPAREN args RPAREN") do |fu, lp, args, rp|
        [nil, args]
      end
    end

    build_nonempty_list_production(:fun_body, :e, :sep)

    production(:fun_def) do
      clause("fun_head sep fun_body sep END") do |head, sep, body, sep, ed|
        args_vars = ((head as Array)[1] as Array).map {|v| Variable.new v}
        exps = (body as Array)
               .reduce([] of Expression) do |a, exp|
          a = a + [exp as Expression]
        end
        Prototype.new((head as Array).first, args_vars as Array, FunBody.new(exps))
      end
    end
    build_nonempty_list_production(:program_instructions, :e, :sep)

    production(:statement) do
      clause("program_instructions") do |e|
        exps = (e as Array).reduce([] of Expression) do |all, x|
          all + [x]
        end.clone
        KProgram.new(exps)
      end
      nil
    end

    finalize
  end
end
