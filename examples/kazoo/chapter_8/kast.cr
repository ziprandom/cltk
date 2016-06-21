# Author:		Chris Wailes <chris.wailes@gmail.com>
# Project: 	Ruby Language Toolkit
# Date:		2011/05/09
# Description:	This file defines a simple AST for the Kazoo language.

# RLTK Files
require "../../../src/cltk/ast"

module Kazoo

  class Expression < CLTK::ASTNode
  end

  class ExpressionList < Expression
    values({expressions: Array(Expression)})
  end

  class ANumber < Expression
    values({value: Float64})
  end

  class Variable < Expression
    values({name: String})
  end

  class Assign < Expression
    values({
             name: String,
             right: Expression
           })
  end

  class Unary < Expression
    values({operand: Expression})
  end

  class Not < Unary; end
  class Neg < Unary; end

  class Binary < Expression
    values({
             right: Expression,
             left: Expression
           })
  end

  class Add < Binary; end
  class Sub < Binary; end
  class Mul < Binary; end
  class Div < Binary; end
  class LT  < Binary; end
  class GT  < Binary; end
  class Eql < Binary; end
  class Or  < Binary; end
  class And < Binary; end

  class Call < Expression
    values({
             name: String,
             args: Array(Expression)
           })
  end

  class If < Expression
    values({
        cond: Expression,
        thenExp: Expression,
        elseExp: Expression
      })
  end

  class For < Expression
    values({
             var: String,
             init: Expression,
             cond: Expression,
             step: Expression,
             body: Expression
           })
  end

  class Prototype < Expression
    values({
             name: String,
             arg_names: Array(String)
           })
  end

  class Function < Expression
    values({
             proto: Prototype,
             body: Expression
           })
  end
end
