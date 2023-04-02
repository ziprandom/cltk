require "../spec_helper"

class ArrayCalc2 < CLTK::Parser
  default_arg_type :array

  production(:e) do
    clause("NUM") { |n| n.as(Array)[0].as(Int32) }

    clause("PLS e e") { |args| args = args.as(Array); args[1].as(Int32) + args[2].as(Int32) }
    clause("SUB e e") { |args| args = args.as(Array); args[1].as(Int32) - args[2].as(Int32) }
    clause("MUL e e") { |args| args = args.as(Array); args[1].as(Int32) * args[2].as(Int32) }
    clause("DIV e e") { |args| args = args.as(Array); args[1].as(Int32) // args[2].as(Int32) }
    nil
  end

  finalize(use: "./spec/cltk/array_calc.bin")
end


describe "Message Pack Bases Parser Serialization" do
  describe ArrayCalc2 do
    actual = ArrayCalc2.parse(CLTK::Lexers::Calculator.lex("+ 1 2"))
    (actual).should eq 3
    actual = ArrayCalc2.parse(CLTK::Lexers::Calculator.lex("+ 1 * 2 3"))
    (actual).should eq 7
    actual = ArrayCalc2.parse(CLTK::Lexers::Calculator.lex("* + 1 2 3"))
    (actual).should eq 9
  end
end
