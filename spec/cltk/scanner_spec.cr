require "../spec_helper"

class CalcLexer < CLTK::Scanner
  # set a delimiter to strip the
  # string before lexing for increased
  # performance. defaults to "\n" which
  # is fine for this example.

  # self.pre_delimiter=nil # no string splitting
  # before lexing

  # ignore space & newline
  rule(/[\n\s]/)

  # operators are keywords, so we use
  # stringrules to have one single
  # dfa matching
  rule("+")                { {:PLS} }
  rule("-")                { {:SUB} }
  rule("*")                { {:MUL} }
  rule("/")                { {:DIV} }

  # ints and floats need to be matched with
  # regular expressions (doesn't use Regex but
  # DFA::RegExp)
  rule(/\d+\.\d+/)         { |s| {:FLOAT, s} }
  rule(/\d+/)              { |s| {:INT,   s} }

  # upon sighting of a '#' (and optionally trailing ' ')
  # we go into :comment state and don't leave until we
  # find a '\n'
  rule(/#\s*/)             {     push_state(:comment) }
  rule(/[^\n]+/, :comment) { |t| {:COMMENT, t}        }
  rule("\n", :comment)     {     pop_state            }
end

source = <<-source
#
# a simple calculation
#

4 + 4  # the first addition
- 3.14 # a substraction
* 3

source


describe CLTK::Scanner do
  subject = CalcLexer.lex(source)

  describe "lex" do

    it "should return an Environment" do
      subject.should be_a(CLTK::Scanner::Environment)
    end

    it "should correctly lex a string" do
      subject.tokens.should eq [{:COMMENT, "a simple calculation"},
                                {:INT, "4"},
                                {:PLS},
                                {:INT, "4"},
                                {:COMMENT,
                                 "the first addition"},
                                {:SUB},
                                {:FLOAT,
                                 "3.14"},
                                {:COMMENT,
                                 "a substraction"},
                                {:MUL},
                                {:INT, "3"}]
    end
  end

  describe "positions" do
    it "should return an array of positions" do
      subject.positions.should eq [{position: 4, size: 20},
                                   {position: 28, size: 1},
                                   {position: 30, size: 1},
                                   {position: 32, size: 1},
                                   {position: 37, size: 18},
                                   {position: 56, size: 1},
                                   {position: 58, size: 4},
                                   {position: 65, size: 14},
                                   {position: 80, size: 1},
                                   {position: 82, size: 1}]
    end
  end

  describe "map" do
    it "should allow on the fly modifying of tokens and positions" do
      subject.map do |t, p|
        {type: t[0], size: p[:size] }
      end.should eq [{type: :COMMENT, size: 20},
                     {type: :INT, size: 1},
                     {type: :PLS, size: 1},
                     {type: :INT, size: 1},
                     {type: :COMMENT, size: 18},
                     {type: :SUB, size: 1},
                     {type: :FLOAT, size: 4},
                     {type: :COMMENT, size: 14},
                     {type: :MUL, size: 1},
                     {type: :INT, size: 1}]
    end

    it "should yield with the environment as context" do
      the_state = nil
      subject.map do
        the_state = state
      end
      the_state.should eq :default
    end
  end
end
