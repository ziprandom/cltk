require "../src/cltk/ast"
require "./kscope"

class CLTK::ASTNode
  def eval_scope(scope)
    eval
  end
end

class Expression < CLTK::ASTNode
end

class KProgram < Expression
  children({
             expressions: Array(CLTK::ASTNode)
           })

  def eval_scope(scope)
    res = Kazoo::Undefined
    (expressions as Array).compact.each do |exp|
      res = (exp as Expression).eval_scope(scope)
    end
    res
  end
end
class ANumber < Expression
  values({
           value: Float64
         })

  def to_s
    value.to_s
  end

end
class KNil < Expression
  def to_s
    "nil"
  end
end

class KBool < Expression; end

class KTrue < KBool
  def to_s
    "true"
  end
end

class KFalse < KBool
  def to_s
    "false"
  end

end
class AHash < Expression
  values({
           dict: Hash(Variable, Expression)
         })
  def eval_scope(scope)
    h = AHash.new(
      (dict as Hash(Variable, Expression)).reduce({} of Variable => Expression) do |memo, key, value|
        (memo as Hash)[key] = (value as Expression).eval_scope(scope) as Expression
        memo
      end
    )
    h
  end

  def to_s
    if (dict as Hash(Variable, Expression)).keys.size == 0
      return "{}"
    end
    "{\n" + (dict as Hash(Variable, Expression)).map do |key, value|
      indented_value = value.to_s.gsub(/\n(.)/) do |s|
        "#{s[0]}  #{s[1]}"
      end

      "  #{key.to_s}: #{indented_value.to_s}"
    end.join(",\n") + "\n}"

  end

end

class AArray < Expression
  children({
             members: Array(Expression)
           })
  def eval_scope(scope)
    AArray.new(
      (members as Array).map do |m|
        if m.is_a? Expression
          m.eval_scope(scope)
        end
      end
    )
  end

  def to_s
    "[ " + (members as Array).map { |m| m.to_s.not_nil! }.join(", ") + " ]"
  end

end

class AString < Expression
  values({
           value: String
         })

  def to_s
    "\"" + value.to_s + "\""
  end
end

class Variable < Expression
  values({
           name: String
         })

  def to_s
    name.to_s
  end

  def eval_scope(scope)
    scope.get(name)
  end
end

class FunCall < Expression

  children({
             prototype_exp: Expression,
             parameters: Array(Expression)
           })
  def eval_scope(scope)
    prototype = (prototype_exp as Expression).eval_scope(scope)
    raise "#{prototype} is not a Function" unless prototype.is_a? Prototype
    param_names = ((prototype as Prototype).args as Array).map do |arg_name|
      (arg_name as Variable).name as String
    end
    param_values = (parameters as Array).map do |exp|
      if exp.responds_to? :eval_scope
        exp.eval_scope(scope)
      else
        exp
      end as Expression
    end
    param_values = (0..(param_names.size-1)).map do |index|
      if param_values[index]?
           param_values[index]
         else
           KNil.new
      end
    end

    param_dict = Hash(String, Expression).zip(
      param_names,
      param_values
    )
    param_scope = Kazoo::Scope(Expression).new(
      (prototype as Prototype).scope,
      param_dict
    )
    res = Kazoo::Undefined
    (((prototype as Prototype).body as FunBody).expressions as Array).each do |exp|
      res = (exp as Expression).eval_scope(param_scope)
    end
    res
  end
end

class Binary < Expression
  children({
             left: Expression,
             right: Expression
           })
end

class KOr < Binary
  def eval_scope(scope)
    l = (left as Expression).eval_scope(scope)
    if l.class == KNil || l.class == KFalse
      (right as Expression).eval_scope(scope)
    else
      l
    end
  end
end

class KAnd < Binary
  def eval_scope(scope)
    l = (left as Expression).eval_scope(scope)
    r = (right as Expression).eval_scope(scope)
    if l.class == KNil || l.class == KFalse || r.class == KNil || r.class == KFalse
      KFalse.new
    else
      KTrue.new
    end
  end
end

class VarAssign < Expression
  children({
             left: Variable,
             right: Expression
           })

  def eval_scope(scope)
    l = left
    r = right as Expression
    if l.is_a? Variable
      if r.responds_to? :eval_scope
        scope[l.name.not_nil!] = (r as Expression).eval_scope(scope) as Expression
      else
        scope[l.name.not_nil!] = (r as Expression)
      end
    end
  end

  def to_s
    (left as Expression).to_s + " << " + (right as Expression).to_s
  end
end

class Add < Binary
  def eval_scope(scope)
    ANumber.new(((left.not_nil! as CLTK::ASTNode).eval_scope(scope) as ANumber).value.not_nil! + ((right.not_nil! as CLTK::ASTNode).eval_scope(scope) as ANumber).value.not_nil!)
  end

  def to_s
    left.to_s + " + " + right.to_s
  end
end
class Sub < Binary
  def eval_scope(scope)
    ANumber.new(((left.not_nil! as CLTK::ASTNode).eval_scope(scope) as ANumber).value.not_nil! - ((right.not_nil! as CLTK::ASTNode).eval_scope(scope) as ANumber).value.not_nil!)
  end
end
class Mul < Binary
  def eval_scope(scope)
    ANumber.new(((left.not_nil! as CLTK::ASTNode).eval_scope(scope) as ANumber).value.not_nil! * ((right.not_nil! as CLTK::ASTNode).eval_scope(scope) as ANumber).value.not_nil!)
  end
end


class Div < Binary
  def eval_scope(scope)
    ANumber.new(((left.not_nil! as CLTK::ASTNode).eval_scope(scope) as ANumber).value.not_nil! / ((right.not_nil! as CLTK::ASTNode).eval_scope(scope) as ANumber).value.not_nil!)
  end
end
class LT  < Binary; end


class Call < Expression
  ORDER = :values
  values({
           name: String
         })

  children({
             args: Array(Expression)
           })
end

class FunBody < CLTK::ASTNode
  children({
    expressions: Array(Expression)
  })
end

class Prototype < Expression
  ORDER = :values
  children({
             args: Array(Variable),
             body: FunBody
  })
  values({
           name: String,
           scope: Kazoo::Scope(Expression)
         })
  def to_s
    "Function #{name}(" + (args as Array).map {|v| (v as Variable).name}.join(',') + ")"
  end

  def eval_scope(sscope)
    self.scope = sscope
    self
  end
end

class Function < CLTK::ASTNode
  children({
             proto: Prototype,
             body: Expression
           })
end
