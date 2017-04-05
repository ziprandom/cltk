# redefine CLTK Types for Broader Values
# (accept Int32 returns in Lexer and
# ParserCallbacks)
module CLTK
  alias TokenValue = (String|Int32)?
end

require "spec"
require "../src/cltk"
