# Author:		Chris Wailes <chris.wailes@gmail.com>
# Project: 	Ruby Language Toolkit
# Date:		2011/05/09
# Description:	This file defines a simple parser for the Kazoo language.

# RLTK Files
require "../../src/cltk/parser"
require "./kast"

module Kazoo
  class Parser < CLTK::Parser

    left :ASSIGN, :FUN, :LPAREN,
         :LBRACK, :LCBRACK,
         :PLUS, :SUB, :MUL, :DIV

    right :RPAREN, :RBRACK, :RCBRACK

    production(:statement) do
      clause("expressions sep*") do |e|
        KProgram.new(e)
      end
      nil
    end

    build_list_production(:expressions, :e, :sep)

    production(:e) do

      clause(:function_call)        { |a| a }

      clause(:fun_def)              { |e| e }
      clause(:varassign)            { |e| e }
      clause(:binary_expressions)   { |binary| binary }

      # composed
      clause(:hash)                 { |hash| hash }
      clause(:array)                { |array| array }

      # in ( )
      clause("LPAREN e RPAREN")     { |_, e, _| e }

      # most basic
      clause(:identifier)           { |var| var}
      clause(:string)               { |string| string }
      clause(:number)               { |n| n}
      clause(:NIL)	            { KNil.new }
      clause(:TRUE)                 { KTrue.new }
      clause(:FALSE)                { KFalse.new }
      nil
    end

    production(:function_call) do
      clause("e LPAREN array_elements RPAREN") do |e, _, arguments, _|
        FunCall.new(e, arguments)
      end
    end

    production(:array) do
      clause("LBRACK array_elements RBRACK") do |_, elements, _|
        a = AArray.new(elements)
      end
    end

    build_list_production(:array_elements, :e, :comma)

    production(:hash) do
      clause("LCBRACK hash_pairs RCBRACK") do |_, hash_pairs, _|
        hash = (hash_pairs as Array).reduce(Hash(Variable, Expression).new) do |hash, pair|
          key, value = pair as Array
          if value.is_a? Expression
            hash[key as Variable] = value
          end
          hash
        end
        AHash.new(hash)
      end
    end

    production(:hash_pair) do
      clause("identifier COLON e") {|e0, _, e1| [e0, e1]}
    end

    build_list_production(:hash_pairs, :hash_pair, :comma)

    production(:binary_expressions) do
      clause("e OR e")          { |e0, _, e2| KOr.new(e0, e2)  }
      clause("e AND e")         { |e0, _, e2| KAnd.new(e0, e2) }
      clause("e PLUS e")	{ |e0, _, e1| Add.new(e0, e1)  }
      clause("e SUB e")	        { |e0, _, e1| Sub.new(e0, e1)  }
      clause("e MUL e")	        { |e0, _, e1| Mul.new(e0, e1)  }
      clause("e DIV e")	        { |e0, _, e1| Div.new(e0, e1)  }
    end

    production(:identifier) do
      clause(:IDENT) { |i| Variable.new(i) }
    end

    production(:string) do
      clause(:STRING)	{ |s| AString.new(s) }
    end

    production(:number) do
      clause(:NUMBER)	{ |n| ANumber.new(n as Float64) }
    end

    production(:varassign) do
      clause("IDENT ASSIGN e") do |ident, _, e|
        VarAssign.new(Variable.new(ident), e)
      end
    end

    production(:sep) do
      clause(:SEMI) { |n| n }
      clause(:CR) { |n| n }
      nil
    end

    production(:comma) do
      clause("CR* COMMA CR*") {|cr, _, _| cr }
    end

    production(:fun_def) do
      clause("fun_head sep fun_body sep END") do |head, _, body, _, _|
        args_vars = ((head as Array)[1] as Array).map {|v| Variable.new v}
        exps = (body as Array)
               .reduce([] of Expression) do |a, exp|
          a = a + [exp as Expression]
        end
        Prototype.new((head as Array).first, args_vars as Array, FunBody.new(exps))
      end
    end

    production(:fun_head) do
      clause("DEF IDENT LPAREN args RPAREN") do |_, ident, _, args, _|
        [ident, args]
      end
      clause("FUN LPAREN args RPAREN") do |_, _, args, _|
        [nil, args]
      end
    end

    build_list_production(:args, :IDENT, :comma)

    build_nonempty_list_production(:fun_body, :e, :sep)

    finalize
  end
end
