require "spec"
require "../../src/cltk/lexer"
require "../../src/cltk/lexers/ebnf"
require "../../src/cltk/lexers/calculator"

class ENVLexer < CLTK::Lexer
  class Environment < Environment
    def initialize(args)
      @value = -1
      super(args)
    end

    def next_value
      @value += 1
      @value as Int32
    end
  end

  rule(/a/) do |txt|
    next {:A, next_value }
  end
end

class ABLongest < CLTK::Lexer
  rule(/a+/)   { :APLUS }
  rule(/b+/)   { :BPLUS }

  rule(/a+b+/) { :APLUSBPLUS }
end

class ABFirst < CLTK::Lexer
  match_first

  rule(/a+/)   { :APLUS }
  rule(/b+/)   { :BPLUS }

  rule(/a+b+/) { :APLUSBPLUS }
end


class FlagLexer < CLTK::Lexer
  rule(/a/)	 { |txt| set_flag(:a); :A }
  rule(/\s/)

  rule(/b/, :default, [:a])     { |txt| set_flag(:b); :B }
  rule(/c/, :default, [:a, :b]) { :C }
end

class StateLexer < CLTK::Lexer
  rule(/a/)    { :A }
  rule(/\s/)

  rule(/\(\*/) { |txt|
    push_state(:comment)
  }

  rule(/\(\*/, :comment) { |txt| push_state(:comment) }
  rule(/\*\)/, :comment) { |txt| pop_state }
  rule(/./,    :comment)
end

class MatchDataLexer < CLTK::Lexer
  rule(/a(b*)(c+)/) do |txt|
    {:FOO,
     [
       match[1]? || "",
       match[2]? || ""
     ]
    }
  end
end

describe "CLTK::Lexer" do

  describe "test_calc" do
    expected = [
      CLTK::Token.new(:NUM, 1),

      CLTK::Token.new(:PLS),
      CLTK::Token.new(:SUB),
      CLTK::Token.new(:MUL),
      CLTK::Token.new(:DIV),

      CLTK::Token.new(:LPAREN),
      CLTK::Token.new(:RPAREN),
      CLTK::Token.new(:EOS)
    ]
    actual = CLTK::Lexers::Calculator.lex("1 + - * / ( )")
    expected.should eq actual
  end

  describe "test_ebnf" do
    expected = [
      CLTK::Token.new(:NONTERM, "aaa"),
      CLTK::Token.new(:TERM, "BBB"),

      CLTK::Token.new(:STAR),
      CLTK::Token.new(:PLUS),
      CLTK::Token.new(:QUESTION),
      CLTK::Token.new(:EOS)
    ]
    actual = CLTK::Lexers::EBNF.lex("aaa BBB * + ?")
    expected.should eq actual
  end

  it "can instantiate a lexer with a custom environment" do
    expected = [
      CLTK::Token.new(:A, 0),
      CLTK::Token.new(:A, 1),
      CLTK::Token.new(:A, 2),
      CLTK::Token.new(:EOS)
    ]

    actual = ENVLexer.lex("aaa")

    expected.should eq actual

    lexer = ENVLexer.new

    expected.should eq lexer.lex("aaa")

    expected = [
      CLTK::Token.new(:A, 3),
      CLTK::Token.new(:A, 4),
      CLTK::Token.new(:A, 5),
      CLTK::Token.new(:EOS)
    ]

    expected.should eq lexer.lex("aaa")

  end


  it "test_first_match" do
    expected = [
      CLTK::Token.new(:APLUS),
      CLTK::Token.new(:BPLUS),
      CLTK::Token.new(:EOS)
    ]

    actual = ABFirst.lex("aaabbb")
    expected.should eq actual
  end

  it "test_flags" do

    expect_raises(CLTK::LexingError) do
      FlagLexer.lex("b")
    end

    expect_raises(CLTK::LexingError) do
      FlagLexer.lex("ac")
    end

    expected = [
      CLTK::Token.new(:A),
      CLTK::Token.new(:B),
      CLTK::Token.new(:C),
      CLTK::Token.new(:EOS)
    ]

    actual = FlagLexer.lex("abc")
    expected.should eq actual

    expected = [
      CLTK::Token.new(:A),
      CLTK::Token.new(:B),
      CLTK::Token.new(:C),
      CLTK::Token.new(:A),
      CLTK::Token.new(:B),
      CLTK::Token.new(:C),
      CLTK::Token.new(:EOS)
    ]

    actual = FlagLexer.lex("abcabc")
    expected.should eq actual
  end

  it "test_lex" do
    expect_raises(CLTK::LexingError) { ABFirst.lex("aaabbbCCC") }
    expect_raises(CLTK::LexingError) { ABLongest.lex("aaabbbCCC") }
  end

  it "test_longest_match" do
    expected = [
      CLTK::Token.new(:APLUSBPLUS),
      CLTK::Token.new(:EOS)
    ]

    actual = ABLongest.lex("aaabbb")

    expected.should eq actual
  end

  it "test_match_data" do
    expected = [CLTK::Token.new(:FOO, ["", "ccc"]), CLTK::Token.new(:EOS)]
    actual   = MatchDataLexer.lex("accc")

    expected.should eq actual
  end

  it "test_state" do
    expected = [
      CLTK::Token.new(:A),
      CLTK::Token.new(:A),
      CLTK::Token.new(:EOS)
    ]

    actual = StateLexer.lex("a (* bbb *) a")
    expected.should eq actual

    actual = StateLexer.lex("a (* b (* ccc *) b *) a")
    expected.should eq actual
  end
end
