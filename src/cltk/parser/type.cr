require "../ast"
module CLTK

  {% if !CLTK.has_constant?("Type") %}
    alias Type = ASTNode | TokenValue | Array(Type)
  {% end %}

  {% unless CLTK.has_constant?("Parser") && Parser.has_constant?("StackType") %}
    class Parser
      alias StackType = CLTK::TokenValue | Tuple(Int32, Array(StackType), Array(StreamPosition)) | Array(StackType)
    end
  {% end %}

end
