require "../../src/cltk/lexer"
module JSON_PARSE
  # The Lexer
  class Lexer < CLTK::Lexer
    # Skip whitespace.
    rule(/\n/)
    rule(/\s/)

    rule(/:/)	  { :COLON   }
    rule(/\[/)	  { :LBRACK  }
    rule(/\]/)	  { :RBRACK  }
    rule(/\{/)	  { :LCBRACK }
    rule(/\}/)	  { :RCBRACK }
    rule(/,/)	  { :COMMA   }

    rule(/true/)  { {:BOOL, 0} }
    rule(/false/) { {:BOOL, 1} }
    rule(/null/)  { {:NULL, nil} }

    # String with included quoted strings
    rule(/"(?:[^"\\]|\\.)*"/) { |t| {:STRING, t[1...-1]}}

    # Numeric rules.
    rule(/\-?\d+/)            { |t| {:INTEGER, t.to_i} }
    rule(/\-?\d+\.\d+/)	      { |t| {:FLOAT, t.to_f} }
  end
end
