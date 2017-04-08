# Author:		Chris Wailes <chris.wailes@gmail.com>
# Project: 	Ruby Language Toolkit
# Date:		2011/04/06
# Description:	This file contains unit tests for the CLTK::Parser class.

############
# Requires #
############
require "../spec_helper"

#######################
# Classes and Modules #
#######################

class FixABLexer < CLTK::Lexer
  rule(/a/) { {:A, 1} }
  rule(/b/) { {:B, 2} }

  rule(/\s/)
end

class FixAlphaLexer < CLTK::Lexer
  rule(/a|A/) { |t| {:A, t}.as(BlockReturn)}
  rule(/b|B/) { |t| {:B, t}.as(BlockReturn)}
  rule(/c|C/) { |t| {:C, t}.as(BlockReturn)}
  rule(/d|D/) { |t| {:D, t}.as(BlockReturn)}

  rule(/,/) { :COMMA }

  rule(/\s/)
end

class FixUnderscoreLexer < CLTK::Lexer
  rule(/\w/) { |t| {:A_TOKEN, t}.as(BlockReturn)}
end

insert_output_of("precompiled parsers") do
  module CLTK
    alias TokenValue = (String|Int32)?
  end
  require "../../src/cltk/parser"
  # require "../../src/cltk/parser/type"
  require "../../src/cltk/parser/crystalize"
  require "../../src/cltk/parsers/prefix_calc"
  require "../../src/cltk/parsers/infix_calc"
  require "../../src/cltk/parsers/postfix_calc"

  CLTK::Parsers::PostfixCalc.crystalize :FixPostfixCalc
  CLTK::Parsers::InfixCalc.crystalize :FixInfixCalc
  CLTK::Parsers::PrefixCalc.crystalize :FixPrefixCalc

  class FixAPlusBParser < CLTK::Parser
    production(:a, "A+ B") do |a, b|
      if a.is_a? Array
        a.size
      end
    end

    finalize
    crystalize
  end

  class FixAQuestionBParser < CLTK::Parser
    production(:a, "A? B") { |a, b| a }
    finalize
    crystalize
  end

  class FixAStarBParser < CLTK::Parser
    production(:a, "A* B") do |a, b|
      a.as(Array).size
    end

    finalize
    crystalize
  end

  class FixAmbiguousParser < CLTK::Parser
    production(:e) do
      clause("NUM") {|n| n.as(Int32)}

      clause("e PLS e") { |e0, op, e1 | e0.as(Int32) + e1.as(Int32) }
      clause("e SUB e") { |e0, op, e1 | e0.as(Int32) - e1.as(Int32) }
      clause("e MUL e") { |e0, op, e1 | e0.as(Int32) * e1.as(Int32) }
      clause("e DIV e") { |e0, op, e1 | e0.as(Int32) / e1.as(Int32) }

    end

    finalize
    crystalize
  end

  class FixArrayCalc < CLTK::Parser
    default_arg_type :array

    production(:e) do
      clause("NUM") { |n| n.as(Array)[0].as(Int32) }

      clause("PLS e e") { |args| args = args.as(Array); args[1].as(Int32) + args[2].as(Int32) }
      clause("SUB e e") { |args| args = args.as(Array); args[1].as(Int32) - args[2].as(Int32) }
      clause("MUL e e") { |args| args = args.as(Array); args[1].as(Int32) * args[2].as(Int32) }
      clause("DIV e e") { |args| args = args.as(Array); args[1].as(Int32) / args[2].as(Int32) }
      nil
    end

    finalize
    crystalize
  end

  # This grammar is purposefully ambiguous.  This should not be equivalent
  # to the grammar produced with `e -> A B? B?`, due to greedy Kleene
  # operators.

  class FixAmbiguousParseStackParser < CLTK::Parser
    production(:s, "e*")

    production(:e, "A b_question b_question") { |a, b0, b1| [a, b0, b1] }

    production(:b_question) do
      clause("")	{ nil }
      clause("B")
      nil
    end

    finalize
    crystalize
  end

  class FixEBNFSelectorParser < CLTK::Parser
    default_arg_type :array

    production(:s) do
      clause(".A .B* .A") { |a| a }
      clause(".B C* .B")  { |a| a }
      nil
    end

    finalize
    crystalize
  end

  class FixEmptyListParser0 < CLTK::Parser
    build_list_production("list", :A, :COMMA)

    finalize
    crystalize
  end

  class FixEmptyListParser1 < CLTK::Parser
    default_arg_type :array

    build_list_production("list", ["A", "B", "C D"], :COMMA)

    finalize
    crystalize
  end

  class FixGreedTestParser0 < CLTK::Parser
    production(:e, "A? A") do |a0, a1|
      [a0, a1]
    end

    finalize
    crystalize
  end

  class FixGreedTestParser1 < CLTK::Parser
    production(:e, "A? A?") do |a0, a1|
      [a0, a1]
    end

    finalize
    crystalize
  end

  class FixGreedTestParser2 < CLTK::Parser
    production(:e, "A* A") { |a0, a1| [a0, a1] }

    finalize
    crystalize
  end

  class FixGreedTestParser3 < CLTK::Parser
    production(:e, "A+ A") { |a0, a1| [a0, a1] }

    finalize
    crystalize
  end

  class FixNonEmptyListParser10 < CLTK::Parser
    build_nonempty_list_production("list", :A, :COMMA)

    finalize
    crystalize
  end

  class FixNonEmptyListParser1 < CLTK::Parser
    build_nonempty_list_production("list", [:A, :B], :COMMA)

    finalize
    crystalize
  end

  class FixNonEmptyListParser2 < CLTK::Parser
    build_nonempty_list_production("list", ["A", "B", "C D"], :COMMA)

    finalize
    crystalize
  end

  class FixNonEmptyListParser3 < CLTK::Parser
    build_nonempty_list_production("list", "A+", :COMMA)

    finalize
    crystalize
  end

  class FixNonEmptyListParser4 < CLTK::Parser
    build_nonempty_list_production("list", :A)

    finalize
    crystalize
  end

  class FixNonEmptyListParser5 < CLTK::Parser
    build_nonempty_list_production("list", :A, "B C?")

    finalize
    crystalize
  end

  class FixErrorCalc < CLTK::Parser
    left :ERROR
    right :PLS, :SUB, :MUL, :DIV, :NUM

    production(:e) do
      clause("NUM") {|n| n}

      clause("e PLS e") { |e0, op, e1| e0.as(Int32) + e1.as(Int32) }
      clause("e SUB e") { |e0, op, e1| e0.as(Int32) - e1.as(Int32) }
      clause("e MUL e") { |e0, op, e1| e0.as(Int32) * e1.as(Int32) }
      clause("e DIV e") { |e0, op, e1| e0.as(Int32) / e1.as(Int32) }
      clause("e PLS ERROR e") do |e0, op, ts, e1|
        error(ts);
        e0.as(Int32) + e1.as(Int32)
      end

      nil

    end

    finalize
    crystalize
  end

  class FixErrorLine < CLTK::Parser
    production(:s, "line*") { |l| l }

    production(:line) do
      clause("NEWLINE") { nil }

      clause("WORD+ SEMI NEWLINE") { |w, semi, newline| w }
      clause("WORD+ ERROR") do |w, err|
        error(
          pos(1).not_nil!.line_number
        )
        w
      end
      nil
    end

    finalize
    crystalize
  end

  class FixUnderscoreParser < CLTK::Parser
    production(:s, "A_TOKEN+") { |o| o }

    finalize
    crystalize
  end

  class FixRotatingCalc < CLTK::Parser

    class Environment < Environment
      @map = { :+ => 0, :- => 1, :* => 2, :/ => 3 }
      @ops : Array(
               Proc(Int32, Int32, Int32)
             ) = [
      ->(a : Int32, b : Int32) { a + b }, # +
      ->(a : Int32, b : Int32) { a - b }, # -
      ->(a : Int32, b : Int32) { a * b }, # *
      ->(a : Int32, b : Int32) { a / b }  # /
    ]

      def get_op(orig_op)
        new_op = @ops[@map[orig_op]]

        @ops = @ops[1..-1] << @ops[0]

        new_op
      end
    end

    production(:e) do
      clause("NUM") {|n| n.as(Int32)}

      clause("PLS e e") { | op, e1, e2| get_op(:+).call(e1.as(Int32), e2.as(Int32)) }
      clause("SUB e e") { | op, e1, e2| get_op(:-).call(e1.as(Int32), e2.as(Int32)) }
      clause("MUL e e") { | op, e1, e2| get_op(:*).call(e1.as(Int32), e2.as(Int32)) }
      clause("DIV e e") { | op, e1, e2| get_op(:/).call(e1.as(Int32), e2.as(Int32)) }
      nil
    end


    finalize
    crystalize
  end

  class FixSelectionParser < CLTK::Parser
    production(:s, "A+ .B+") do |bs|
      bs.as(Array).reduce(0) do |sum, add|
        sum + add.as(Int32)
      end
    end

    finalize
    crystalize
  end


  class FixUselessParser < CLTK::Parser
    production(:s, "A+") { |a| a }
    crystalize
  end

  class FixTokenHookParser < CLTK::Parser
    default_arg_type :array

    class Environment < Environment
      property :counter

      def initialize
        @counter = 0
        super
      end
    end

    production(:s) do
      clause("A A A A") { nil }
      clause("B B B B") { nil }
      nil
    end

    token_hook(:A) { |env| env.as(Environment).counter += 1; next nil }
    token_hook(:B) { |env| env.as(Environment).counter += 2; next nil }

    finalize
    crystalize
  end

end

class DummyError1 < Exception; end
class DummyError2 < Exception; end

class FixELLexer < CLTK::Lexer
  rule(/\n/) { :NEWLINE }
  rule(/\;/)  { :SEMI    }

  rule(/\s/)

  rule(/[A-Za-z]+/) { |t| {:WORD, t}.as(BlockReturn)}
end

#
# we have to redefine the environment in this
# compilation context/run
#
module FixRotatingCalc
  class Environment < CLTK::Parser::Environment
    @map = { :+ => 0, :- => 1, :* => 2, :/ => 3 }
    @ops : Array(
             Proc(Int32, Int32, Int32)
           ) = [
    ->(a : Int32, b : Int32) { a + b }, # +
    ->(a : Int32, b : Int32) { a - b }, # -
    ->(a : Int32, b : Int32) { a * b }, # *
    ->(a : Int32, b : Int32) { a / b }  # /
  ]

    def get_op(orig_op)
      new_op = @ops[@map[orig_op]]
      @ops = @ops[1..-1] << @ops[0]
      new_op
    end
  end
end

module FixTokenHookParser
  class Environment < CLTK::Parser::Environment
    property :counter

    def initialize
      @counter = 0
      super
    end
  end
end

describe "CLTK::Parser" do

  it "test_ambiguous_grammar" do
    actual = FixAmbiguousParser.parse(CLTK::Lexers::Calculator.lex("1 + 2 * 3"), {accept: :all})
    actual.should eq [9,7]
  end

  # This test is to ensure that objects placed on the output stack are
  # cloned when we split the parse stack.  This was posted as Issue #17 on
  # Github.
  it "test_ambiguous_parse_stack" do
    FixAmbiguousParseStackParser.parse(FixABLexer.lex("ab")).as(Array).size.should eq 1
  end

  it "test_array_args" do
    actual = FixArrayCalc.parse(CLTK::Lexers::Calculator.lex("+ 1 2"))
    (actual).should eq 3
    actual = FixArrayCalc.parse(CLTK::Lexers::Calculator.lex("+ 1 * 2 3"))
    (actual).should eq 7
    actual = FixArrayCalc.parse(CLTK::Lexers::Calculator.lex("* + 1 2 3"))
    (actual).should eq 9
  end

  #  it "test_construction_error" do
  #    expect_raises(CLTK::Parser::Exceptions::ParserConstructionException) do
  #      class FixMyClass < CLTK::Parser
  #  	finalize
  #      end
  #    end
  #  end

  it "test_ebnf_parsing" do
    ################
    # APlusBParser #
    ################

    expect_raises(CLTK::Parser::Exceptions::NotInLanguage) { FixAPlusBParser.parse(FixABLexer.lex("b")) }

    (FixAPlusBParser.parse(FixABLexer.lex("ab"))).should eq 1
    (FixAPlusBParser.parse(FixABLexer.lex("aab"))).should eq 2
    (FixAPlusBParser.parse(FixABLexer.lex("aaab"))).should eq 3
    (FixAPlusBParser.parse(FixABLexer.lex("aaaab"))).should eq 4

    ####################
    # AQuestionBParser #
    ####################

    expect_raises(CLTK::Parser::Exceptions::NotInLanguage) { FixAQuestionBParser.parse(FixABLexer.lex("aab")) }
    FixAQuestionBParser.parse(FixABLexer.lex("b")).should be_nil
    FixAQuestionBParser.parse(FixABLexer.lex("ab")).should_not be_nil

    ################
    # AStarBParser #
    ################

    FixAStarBParser.parse(FixABLexer.lex("b")).as(Int32).should eq 0
    FixAStarBParser.parse(FixABLexer.lex("ab")).as(Int32).should eq 1
    FixAStarBParser.parse(FixABLexer.lex("aab")).as(Int32).should eq 2
    FixAStarBParser.parse(FixABLexer.lex("aaab")).as(Int32).should eq 3
    FixAStarBParser.parse(FixABLexer.lex("aaaab")).as(Int32).should eq 4
  end

  it "test_empty_list" do
    ####################
    # EmptyListParser0 #
    ####################

    expected = [] of CLTK::Type
    actual   = FixEmptyListParser0.parse(FixAlphaLexer.lex(""))
    actual.should eq(expected)

    ####################
    # EmptyListParser1 #
    ####################

    expected = ["a", "b", ["c", "d"]]
    actual   = FixEmptyListParser1.parse(FixAlphaLexer.lex("a, b, c d"))
    actual.should eq(expected)
  end

  it "test_greed" do

    ####################
    # GreedTestParser0 #
    ####################

    expected = [nil, "a"]
    actual   = FixGreedTestParser0.parse(FixAlphaLexer.lex("a"))
    actual.should eq expected

    expected = ["a", "a"]
    actual   = FixGreedTestParser0.parse(FixAlphaLexer.lex("a a"))
    actual.should eq expected

    ####################
    # GreedTestParser1 #
    ####################
    expected = [nil, nil]
    actual   = FixGreedTestParser1.parse(FixAlphaLexer.lex(""))
    actual.should eq expected

    expected = ["a", nil]
    expected = [nil, "a"]
    actual   = FixGreedTestParser1.parse(FixAlphaLexer.lex("a"))
    actual.should eq expected



    expected = ["a", "a"]
    actual   = FixGreedTestParser1.parse(FixAlphaLexer.lex("a a"))
    actual.should eq expected


    ####################
    # GreedTestParser2 #
    ####################

    expected = [[] of CLTK::Type, "a"]
    actual   = FixGreedTestParser2.parse(FixAlphaLexer.lex("a"))
    actual.should eq expected


    expected = [["a"], "a"]
    actual   = FixGreedTestParser2.parse(FixAlphaLexer.lex("a a"))
    actual.should eq expected



    expected = [["a", "a"], "a"]
    actual   = FixGreedTestParser2.parse(FixAlphaLexer.lex("a a a"))
    actual.should eq expected


    ####################
    # GreedTestParser3 #
    ####################

    expected = [["a"], "a"]
    actual   = FixGreedTestParser3.parse(FixAlphaLexer.lex("a a"))
    actual.should eq expected

    expected = [["a", "a"], "a"]
    actual   = FixGreedTestParser3.parse(FixAlphaLexer.lex("a a a"))
    actual.should eq expected

  end

  it "test_ebnf_selector_interplay" do
    expected = ["a", ["b", "b", "b"], "a"]
    actual   = FixEBNFSelectorParser.parse(FixAlphaLexer.lex("abbba"))
    actual.should eq expected

    expected = ["a", [] of CLTK::Type, "a"]
    actual   = FixEBNFSelectorParser.parse(FixAlphaLexer.lex("aa"))
    actual.should eq expected

    expected = ["b", "b"]
    actual   = FixEBNFSelectorParser.parse(FixAlphaLexer.lex("bb"))
    actual.should eq expected

    expected = ["b", "b"]
    actual   = FixEBNFSelectorParser.parse(FixAlphaLexer.lex("bcccccb"))
    actual.should eq expected

  end

  it "test_environment" do
    actual = FixRotatingCalc.parse(CLTK::Lexers::Calculator.lex("+ 1 2"))
    actual.should eq 3

    actual = FixRotatingCalc.parse(CLTK::Lexers::Calculator.lex("/ 1 * 2 3"))
    actual.should eq 7

    actual = FixRotatingCalc.parse(CLTK::Lexers::Calculator.lex("- + 1 2 3"))
    actual.should eq 9

    parser = FixRotatingCalc.new

    actual = parser.parse(CLTK::Lexers::Calculator.lex("+ 1 2"))
    actual.should eq 3

    actual = parser.parse(CLTK::Lexers::Calculator.lex("/ 1 2"))
    actual.should eq 3
  end

  it "test_error_productions" do

    # Test to see if error reporting is working correctly.

    test_string  = "first line;\n"
    test_string += "second line\n"
    test_string += "third line;\n"
    test_string += "fourth line\n"

    expect_raises(CLTK::Parser::Exceptions::HandledError) { FixErrorLine.parse(FixELLexer.lex(test_string)) }

    # Test to see if we can continue parsing after errors are encounterd.
    begin
      FixErrorLine.parse(FixELLexer.lex(test_string))
    rescue ex : CLTK::Parser::Exceptions::HandledError
      ex.errors.should eq [2,4]
    end

    begin
      FixErrorCalc.parse(CLTK::Lexers::Calculator.lex("1 + + 1"))
    rescue ex : CLTK::Parser::Exceptions::HandledError
      ex.errors.first.as(Array).size.should eq 1
      ex.result.should eq 2
    end

    # Test to see if we pop tokens correctly after an error is
    # encountered.
    begin
      FixErrorCalc.parse(CLTK::Lexers::Calculator.lex("1 + + + + + + 1"))
    rescue ex : CLTK::Parser::Exceptions::HandledError
      ex.errors.first.as(Array).size.should eq 5
      ex.result.should eq 2
    end
  end

  it "test_infix_calc" do
    actual = FixInfixCalc.parse(CLTK::Lexers::Calculator.lex("1 + 2"))
    actual.should eq 3

    actual = FixInfixCalc.parse(CLTK::Lexers::Calculator.lex("1 + 2 * 3"))
    actual.should eq 7

    actual = FixInfixCalc.parse(CLTK::Lexers::Calculator.lex("(1 + 2) * 3"))
    actual.should eq 9

    expect_raises(CLTK::Parser::Exceptions::NotInLanguage) { FixInfixCalc.parse(CLTK::Lexers::Calculator.lex("1 2 + 3 *")) }
  end

  it "test_input" do
    expect_raises(CLTK::Parser::Exceptions::BadToken) { FixInfixCalc.parse(CLTK::Lexers::EBNF.lex("A B C")) }
  end

  it "test_nonempty_list" do
    #######################
    # NonEmptyListParser10 #
    #######################

    expected = ["a"]
    actual   = FixNonEmptyListParser10.parse(FixAlphaLexer.lex("a"))
    actual.should eq expected

    expected = ["a", "a"]
    actual   = FixNonEmptyListParser10.parse(FixAlphaLexer.lex("a, a"))
    actual.should eq expected

    expect_raises(CLTK::Parser::Exceptions::NotInLanguage) { FixNonEmptyListParser10.parse(FixAlphaLexer.lex(""))   }
    expect_raises(CLTK::Parser::Exceptions::NotInLanguage) { FixNonEmptyListParser10.parse(FixAlphaLexer.lex(","))  }

    expect_raises(CLTK::Parser::Exceptions::NotInLanguage) { FixNonEmptyListParser10.parse(FixAlphaLexer.lex("aa")) }
    expect_raises(CLTK::Parser::Exceptions::NotInLanguage) { FixNonEmptyListParser10.parse(FixAlphaLexer.lex("a,")) }
    expect_raises(CLTK::Parser::Exceptions::NotInLanguage) { FixNonEmptyListParser10.parse(FixAlphaLexer.lex(",a")) }

    #######################
    # NonEmptyListParser1 #
    #######################
    expected = ["a"]
    actual   = FixNonEmptyListParser1.parse(FixAlphaLexer.lex("a"))
    actual.should eq expected
    expected = ["b"]
    actual   = FixNonEmptyListParser1.parse(FixAlphaLexer.lex("b"))
    actual.should eq expected

    expected = ["a", "b", "a", "b"]
    actual   = FixNonEmptyListParser1.parse(FixAlphaLexer.lex("a, b, a, b"))
    actual.should eq expected

    expect_raises(CLTK::Parser::Exceptions::NotInLanguage) { FixNonEmptyListParser1.parse(FixAlphaLexer.lex("a b")) }
    expect_raises(CLTK::Parser::Exceptions::NotInLanguage) { FixNonEmptyListParser1.parse(FixAlphaLexer.lex("a, ")) }

    #######################
    # NonEmptyListParser2 #
    #######################

    expected = ["a"]
    actual   = FixNonEmptyListParser2.parse(FixAlphaLexer.lex("a"))
    actual.should eq expected

    expected = ["b"]
    actual   = FixNonEmptyListParser2.parse(FixAlphaLexer.lex("b"))
    actual.should eq expected

    expected = [["c", "d"]]
    actual   = FixNonEmptyListParser2.parse(FixAlphaLexer.lex("c d"))
    actual.should eq expected

    expected = [["c", "d"], ["c", "d"]]
    actual   = FixNonEmptyListParser2.parse(FixAlphaLexer.lex("c d, c d"))
    actual.should eq expected

    expected = ["a", "b", ["c", "d"]]
    actual   = FixNonEmptyListParser2.parse(FixAlphaLexer.lex("a, b, c d"))
    actual.should eq expected

    expect_raises(CLTK::Parser::Exceptions::NotInLanguage) { FixNonEmptyListParser2.parse(FixAlphaLexer.lex("c")) }
    expect_raises(CLTK::Parser::Exceptions::NotInLanguage) { FixNonEmptyListParser2.parse(FixAlphaLexer.lex("d")) }

    #######################
    # NonEmptyListParser3 #
    #######################

    expected = [["a"], ["a", "a"], ["a", "a", "a"]]
    actual   = FixNonEmptyListParser3.parse(FixAlphaLexer.lex("a, aa, aaa"))
    actual.should eq expected

    #######################
    # NonEmptyListParser4 #
    #######################

    expected = ["a", "a", "a"]
    actual   = FixNonEmptyListParser4.parse(FixAlphaLexer.lex("a a a"))
    actual.should eq expected

    #######################
    # NonEmptyListParser5 #
    #######################

    expected = ["a", "a", "a"]
    actual   = FixNonEmptyListParser5.parse(FixAlphaLexer.lex("a b a b c a"))
    actual.should eq expected

    expect_raises(CLTK::Parser::Exceptions::NotInLanguage) { FixNonEmptyListParser5.parse(FixAlphaLexer.lex("a b b a")) }
  end

  it "test_postfix_calc" do
    actual = FixPostfixCalc.parse(CLTK::Lexers::Calculator.lex("1 2 +"))
    actual.should eq 3

    actual = FixPostfixCalc.parse(CLTK::Lexers::Calculator.lex("1 2 3 * +"))
    actual.should eq 7

    actual = FixPostfixCalc.parse(CLTK::Lexers::Calculator.lex("1 2 + 3 *"))
    actual.should eq 9

    expect_raises(CLTK::Parser::Exceptions::NotInLanguage) { FixInfixCalc.parse(CLTK::Lexers::Calculator.lex("* + 1 2 3")) }
  end

  it "test_prefix_calc" do
    actual = FixPrefixCalc.parse(CLTK::Lexers::Calculator.lex("+ 1 2"))
    actual.should eq 3

    actual = FixPrefixCalc.parse(CLTK::Lexers::Calculator.lex("+ 1 * 2 3"))
    actual.should eq 7

    actual = FixPrefixCalc.parse(CLTK::Lexers::Calculator.lex("* + 1 2 3"))
    actual.should eq 9

    expect_raises(CLTK::Parser::Exceptions::NotInLanguage) { FixPrefixCalc.parse(CLTK::Lexers::Calculator.lex("1 + 2 * 3")) }
  end

  it "test_selection_parser" do
    actual   = FixSelectionParser.parse(FixABLexer.lex("aaabbb"))
    expected = 6

    actual.should eq expected
  end

  pending "test_token_hooks" do
    parser = FixTokenHookParser.new

    parser.parse(FixAlphaLexer.lex("a a a a"))
    parser.env.counter.should eq 4

    parser.parse(FixAlphaLexer.lex("b b b b"))
    parser.env.counter.should eq 12
  end

  it "test_underscore_tokens" do
    actual   = FixUnderscoreParser.parse(FixUnderscoreLexer.lex("abc")).as(Array).join
    expected = "abc"

    actual.should eq expected
  end

  it "test_useless_parser_exception" do
    expect_raises(CLTK::Parser::Exceptions::UselessParserException) { FixUselessParser.new }
  end
end
