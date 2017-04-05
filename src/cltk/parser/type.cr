{% if !CLTK.has_constant?("Type") %}
require "../ast"
module CLTK
  alias Type = ASTNode | TokenValue | Array(Type)
end
{% end %}
