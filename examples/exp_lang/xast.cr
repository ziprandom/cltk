require "../../src/cltk/ast"
require "./xscope"

class CLTK::ASTNode
  def eval_scope(scope)
    eval
  end
end

class Expression < CLTK::ASTNode
end

class XProgram < Expression
  values({
             expressions: Array(CLTK::ASTNode)
           })

  def eval_scope(scope)
    expressions.compact.reduce(EXP_LANG::Undefined) do |lastResult, exp|
      exp.eval_scope(scope)
    end
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
    h = AHash.new(dict: dict.reduce({} of Variable => Expression) do |memo, pair|
        memo[pair[0]] = pair[1].eval_scope(scope) as Expression
        memo
      end
    )
    h
  end

  def to_s
    if dict.keys.size == 0
      return "{}"
    end
    "{\n" + dict.map do |key, value|
      indented_value = value.to_s.gsub(/\n(.)/) do |s|
        "#{s[0]}  #{s[1]}"
      end

      "  #{key.to_s}: #{indented_value.to_s}"
    end.join(",\n") + "\n}"

  end

end

class AArray < Expression
  values({
             members: Array(Expression)
           })
  def eval_scope(scope)
    AArray.new(members:
      members.map do |m|
        if m.is_a? Expression
          m.eval_scope(scope)
        end
      end
    )
  end

  def to_s
    "[ " + members.map { |m| m.to_s.not_nil! }.join(", ") + " ]"
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

  values({
             prototype_exp: Expression,
             parameters: Array(Expression)
           })
  def eval_scope(scope)
    prototype = prototype_exp.eval_scope(scope)
    raise "#{prototype} is not a Function" unless prototype.is_a? Prototype
    param_names = prototype.args.map do |arg_name|
      arg_name.name as String
    end
    param_values = parameters.map do |exp|
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
    param_scope = EXP_LANG::Scope(Expression).new(
      prototype.scope,
      param_dict
    )
    res = EXP_LANG::Undefined
    prototype.body.expressions.each do |exp|
      res = exp.eval_scope(param_scope)
    end
    res
  end
end

class Binary < Expression
  values({
             left: Expression,
             right: Expression
           })
end

class KOr < Binary
  def eval_scope(scope)
    l = left.eval_scope(scope)
    if l.class == KNil || l.class == KFalse
      right.eval_scope(scope)
    else
      l
    end
  end
end

class KAnd < Binary
  def eval_scope(scope)
    l = left.eval_scope(scope)
    r = right.eval_scope(scope)
    if l.class == KNil || l.class == KFalse || r.class == KNil || r.class == KFalse
      KFalse.new
    else
      KTrue.new
    end
  end
end

class VarAssign < Expression
  values({
             left: Variable,
             right: Expression
           })

  def eval_scope(scope)
    if left.is_a? Variable
      if right.responds_to? :eval_scope
        scope[left.name.not_nil!] = right.eval_scope(scope) as Expression
      else
        scope[left.name.not_nil!] = right
      end
    end
  end

  def to_s
    left.to_s + " << " + right.to_s
  end
end

class Add < Binary
  def eval_scope(scope)
    ANumber.new(
      value: (left.eval_scope(scope) as ANumber).value +
      (right.eval_scope(scope) as ANumber).value
    )
  end

  def to_s
    left.to_s + " + " + right.to_s
  end
end
class Sub < Binary
  def eval_scope(scope)
    ANumber.new(value: (left.eval_scope(scope) as ANumber).value - (right.eval_scope(scope) as ANumber).value)
  end
end
class Mul < Binary
  def eval_scope(scope)
    ANumber.new(value: (left.eval_scope(scope) as ANumber).value * (right.eval_scope(scope) as ANumber).value)
  end
end


class Div < Binary
  def eval_scope(scope)
    ANumber.new(value: (left.eval_scope(scope) as ANumber).value / (right.eval_scope(scope) as ANumber).value)
  end
end
class LT  < Binary; end


class Call < Expression

  values({
             name: String,
             args: Array(Expression)
           })
end

class FunBody < CLTK::ASTNode
  values({
    expressions: Array(Expression)
  })
end

class Prototype < Expression
  values({
             args: Array(Variable),
             body: FunBody,
             name: String | Nil,
             scope: EXP_LANG::Scope(Expression) | Nil

  })

  def to_s
    "Function #{name}(" + args.map {|v| v.name}.join(',') + ")"
  end

  def eval_scope(sscope)
    if name
      sscope[name.not_nil!] = self
    end
    self.scope = sscope
    self
  end
end

class Function < CLTK::ASTNode
  values({
             proto: Prototype,
             body: Expression
           })
end
