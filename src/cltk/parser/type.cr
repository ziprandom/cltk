require "../ast"
module CLTK
  alias Type = ASTNode     |
               Token       |
               String      |
               Int32       |
               Float64     |
               Nil         |
               Array(Type) |
               Hash(String, Type)
end
