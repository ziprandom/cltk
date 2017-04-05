# redefine CLTK Types for Broader Values
# (accept Int32 returns in Lexer and
# ParserCallbacks) (we check if it's already
# been set, say for example by the parser_spec.cr
# if all tests run together

require "../src/cltk/ast"
module CLTK
  alias TokenValue = (String|Int32)?
end
