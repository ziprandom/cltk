# Description:	This file defines a simple lexer for the EXP_LANG language.

# RLTK Files
module CLTK
  alias TokenValue = (String|Int32|Float64)?
end
require "../../src/cltk/scanner"

module EXP_LANG
  class Lexer < CLTK::Scanner
    extend CLTK::Scanner::LexerCompatibility

                # Skip whitespace.
		rule("\n")	{ { :CR     } }
		rule(" ")

		# Keywords
		rule("def")	{ { :DEF    } }
		rule("fun")	{ { :FUN    } }
		rule("do")	{ { :DO     } }
		rule("end")	{ { :END    } }
		rule("extern")	{ { :EXTERN } }
		rule("nil")	{ { :NIL    } }
		rule("true")	{ { :TRUE   } }
		rule("false")	{ { :FALSE  } }
                rule(".")       { { :DOT    } }
		# Operators and delimiters.
		rule("=")	{ { :ASSIGN } }
		rule("(")	{ { :LPAREN } }
		rule(")")	{ { :RPAREN } }
		rule("[")	{ { :LBRACK } }
		rule("]")	{ { :RBRACK } }
                rule("{")	{ { :LCBRACK } }
		rule("}")	{ { :RCBRACK } }
		rule(":")	{ { :COLON   } }
		rule(";")	{ { :SEMI   } }
		rule(",")	{ { :COMMA  } }
		rule("+")	{ { :PLUS   } }
		rule("-")	{ { :SUB    } }
		rule("\*")	{ { :MUL    } }
		rule("/")	{ { :DIV    } }
		rule("<")	{ { :LT     } }
		rule("||")	{ { :OR     } }
		rule("&&")	{ { :AND    } }

                # Control Flow
                rule("if")   { { :IF    } }
                rule("then") { { :THEN  } }
                rule("else") { { :ELSE  } }

		# Identifier rule.
		rule(/"[^\"]*"/) { |t|        {:STRING, t[1...-1]} }
		rule(/[A-Za-z][_A-Za-z0-9]*/) { |t| {:IDENT, t} }

		# Numeric rules.
		rule(/\d+/)		{ |t| {:NUMBER, t.to_f} }
		rule(/\.\d+/)		{ |t| {:NUMBER, t.to_f} }
		rule(/\d+\.\d+/)	{ |t| {:NUMBER, t.to_f} }

		# Comment rules.
		rule(/#/)				{ |a| push_state :comment }
		rule(/\n/, :comment) do |a|
                  pop_state
                  { :CR }
                end

		rule(/./, :comment)
	end
end
