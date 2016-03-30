# Description:	This file defines a simple parser for the Kazoo language.

# RLTK Files
require "../src/cltk/parser"
require "./kast"

module Kazoo
  class Parser < CLTK::Parser
    default_arg_type :array

    left :ASSIGN
    left :FUN
    left :LPAREN, :LBRACK, :LCBRACK
    right :RPAREN, :RBRACK, :RCBRACK
    left :PLUS, :SUB
    left :MUL, :DIV


    production(:input, "statement") { |s| s[0] }

    production(:sep) do
      clause("SEMI") { |n| n[0] }
      clause("CR") { |n| n[0] }
      nil
    end
    production(:commax) do
      clause("CR* COMMA CR*") {|e| e[1]}
    end
    production("hash_pair") do
      clause("IDENT COLON e") {|e| [e[0], e[2]]}
    end
    build_list_production(:hash_pairs, :hash_pair, :commax)
    build_list_production(:array_elements, :e, :commax)

    production(:e) do
      clause("LPAREN e RPAREN") { |e| e[1] }
      clause("NUMBER")	{ |n| ANumber.new(n[0] as Float64) }
      clause("IDENT")	        { |i| Variable.new(i[0]); }
      clause("NIL")	        { KNil.new }
      clause("TRUE")            { KTrue.new }
      clause("FALSE")           { KFalse.new }
      clause("e OR e")          { |e| KOr.new(e[0], e[2]) as Expression}
      clause("e AND e")         { |e| KAnd.new(e[0], e[2])}
      clause("IDENT")	        { |i| Variable.new(i[0]); }
      clause("e LPAREN array_elements RPAREN") do |e|
        parameters = (e[2] as Array).reduce([] of Expression) do |all, e|
          all + [e as Expression]
        end
        FunCall.new(e[0], parameters)
      end

      clause("STRING")	{ |s| AString.new(s[0]) }
      clause("e PLUS e")	{ |e| a = Add.new(e[0], e[2]); a }
      clause("e SUB e")	{ |e| Sub.new(e[0], e[2]) }
      clause("e MUL e")	{ |e| Mul.new(e[0], e[2]) }
      clause("e DIV e")	{ |e| Div.new(e[0], e[2]) }
      clause("LCBRACK hash_pairs RCBRACK") do |v|
        hash = Hash(Variable, Expression).new
        pairs = (v[1] as Array).reduce([] of Expression) do |all, e |
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
      clause("LBRACK array_elements RBRACK") do |v|
        s = (v[1] as Array).reduce([] of Expression) do |all, e |
          all + [e as Expression]
        end
        a = AArray.new(s)
      end

      clause("fun_def") { |e| e.first }
      clause("varassign") { |e| e[0] }

      nil
    end

    production(:varassign) do
      clause("IDENT ASSIGN e") do |e|
        VarAssign.new(Variable.new(e[0]), e[2])
      end
    end

    build_list_production("args", "IDENT", :commax)

    production(:fun_head) do
      clause("DEF IDENT LPAREN args RPAREN") do |e|
        [e[1], e[3]]
      end
      clause("FUN LPAREN args RPAREN") do |e|
        [nil, e[2]]
      end
    end

    build_nonempty_list_production(:fun_body, :e, :sep)

    production(:fun_def) do
      clause("fun_head sep fun_body sep END") do |e|
        args_vars = ((e[0] as Array)[1] as Array).map {|v| Variable.new v}
        exps = (e[2] as Array)
               .reduce([] of Expression) do |a, exp|
          a = a + [exp as Expression]
        end
        Prototype.new((e[0] as Array).first, args_vars as Array, FunBody.new(exps))
      end
    end
    build_nonempty_list_production(:program_instructions, :e, :sep)

    production(:statement) do
      clause("program_instructions") do |e|
        exps = (e[0] as Array).reduce([] of Expression) do |all, x|
          all + [x]
        end.clone
        KProgram.new(exps)
      end
      nil
    end

    finalize
  end
end
