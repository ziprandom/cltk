require "../../src/cltk/ast"
# The AST Nodes

abstract class JsonExpression < CLTK::ASTNode;  end

class JsonBool < JsonExpression
  values({
           bool: Bool
         })
end

class JsonNull < JsonExpression; end

abstract class JsonNumber < JsonExpression
         end

class JsonInteger < JsonNumber
  values({
           number: Int32
         })
end

class JsonFloat < JsonNumber
  values({
           number: Float64
         })
end

class JsonString < JsonExpression
  values({
           string: String
         })
end

class JsonObject < JsonExpression
  values({
           hash: Hash(String, JsonExpression),
         })
end

class JsonArray < JsonExpression
  values({
           elements: Array(JsonExpression),
         })
end
