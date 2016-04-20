# Description:	This file defines a simple lexer for the Kazoo language.

# RLTK Files
require "../../src/cltk/lexer"

module Kazoo
	class Lexer < CLTK::Lexer
		# Skip whitespace.
		rule(/\n/)		{ :CR  }
		rule(/\s/)

		# Keywords
		rule(/def/)	{ :DEF    }
		rule(/fun/)	{ :FUN    }
		rule(/do/)	{ :DO     }
		rule(/end/)	{ :END    }
		rule(/extern/)	{ :EXTERN }
		rule(/nil/)	{ :NIL    }
		rule(/true/)	{ :TRUE   }
		rule(/false/)	{ :FALSE  }
                rule(/\./)      { :DOT    }
		# Operators and delimiters.
		rule(/\=/)	{ :ASSIGN }
		rule(/\(/)	{ :LPAREN }
		rule(/\)/)	{ :RPAREN }
		rule(/\[/)	{ :LBRACK }
		rule(/\]/)	{ :RBRACK }
                rule(/\{/)	{ :LCBRACK }
		rule(/\}/)	{ :RCBRACK }
		rule(/:/)	{ :COLON   }
		rule(/;/)	{ :SEMI   }
		rule(/,/)	{ :COMMA  }
		rule(/\+/)	{ :PLUS   }
		rule(/-/)	{ :SUB    }
		rule(/\*/)	{ :MUL    }
		rule(/\//)	{ :DIV    }
		rule(/</)	{ :LT     }
		rule(/\|\|/)	{ :OR     }
		rule(/\&\&/)	{ :AND    }

                # Control Flow
                rule(/if/)   { :IF     }
                rule(/then/) { :THEN   }
                rule(/else/) { :ELSE   }

		# Identifier rule.
		rule(/"[^\"]*"/) { |t| {:STRING, t[1...-1]} as BlockReturn}
		rule(/[A-Za-z][_A-Za-z0-9]*/) { |t| {:IDENT, t} as BlockReturn}

		# Numeric rules.
		rule(/\d+/)		{ |t| {:NUMBER, t.to_f} as BlockReturn}
		rule(/\.\d+/)		{ |t| {:NUMBER, t.to_f} as BlockReturn}
		rule(/\d+\.\d+/)	{ |t| {:NUMBER, t.to_f} as BlockReturn}

		# Comment rules.
		rule(/#/)				{ |a, e| e.push_state :comment }
		rule(/\n/, :comment) do |a,e|
                  e.pop_state
                  :CR
                end

		rule(/./, :comment)
	end
end
