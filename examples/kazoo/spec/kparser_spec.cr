require "spec"
require "../klexer"
require "../kparser"

pending "Kazoo::Parser::Comments" do
  lexer = Kazoo::Lexer
  parser = Kazoo::Parser
  string = "1 + 2 # simple addition \n" \
           "# one long comment spanning \n" \
           "# several lines \n"
  tokens = lexer.lex(string)
  it "parses a programm ignoring commented lines" do
    pp tokens.map &.type
    res = parser.parse(tokens, {:accept => :first})
    puts "i am here"

    res.class.should eq Add
    left = (res as Add).left as ANumber
    right = (res as Add).right as ANumber
    left.class.should eq ANumber
    left.value.should eq 1
    right.class.should eq ANumber
    right.value.should eq 2
  end
end

describe "Kazoo::Parser::VariableAssignment" do
  lexer = Kazoo::Lexer
  parser = Kazoo::Parser
  string = "b = 123 + 3"
  tokens = lexer.lex(string)
  res = ((parser.parse(tokens, {:accept => :first}) as KProgram).expressions as Array).first
  it "should parse an Assignment as such" do
    res.class.should eq VarAssign
  end

  it "should parse left" do
    left = (res as VarAssign).left
    left.class.should eq Variable
    (left as Variable).name.should eq "b"
  end

  it "should parse right" do
    right = (res as VarAssign).right
    right.class.should eq Add
    rright = (right as Add).right as ANumber
    rleft = (right as Add).left as ANumber
    rleft.value.should eq 123.to_f
    rright.value.should eq 3.to_f
  end

  it "should parse different right hands" do

    srcs = {
      "a = 1" => "a << 1",
      "a = 1 + 1" => "a << 1 + 1",
      "a = [1,2,3]" => "a << [ 1, 2, 3 ]",
      "a = [1,2,3] + [3,2,1]" => "a << [ 1, 2, 3 ] + [ 3, 2, 1 ]",
      "a = [1,2,3, def add(x,y); x + y; end]" => "a << [ 1, 2, 3, Function add(x,y) ]"
    }

    srcs.each do |src, s|
      lexer = Kazoo::Lexer
      parser = Kazoo::Parser
      tokens = lexer.lex(src)
      res = ((parser.parse(tokens, {:accept => :first}) as KProgram).expressions as Array).first
      res.class.should eq VarAssign
      res.to_s.should eq s
    end
  end
end
describe "Kazoo::Parser::FunDef" do
  lexer = Kazoo::Lexer
  parser = Kazoo::Parser
  string = "def stuff(a,b,c) \n" \
           "10+2\n" \
           "3-2 \n" \
           "76*23 \n" \
           "[def stuffin(x,y) \n" \
             "1 + 1\n" \
           "end]\n" \
           "end"
#  string = "def stuff(a,b,c);10+2;end"
  tokens = lexer.lex(string)
#  pp tokens.map &.type
  res = ((parser.parse(tokens, {:accept => :first}) as KProgram).expressions as Array).first

  it "should parse a list of expressions as a Prototype" do
      res.class.should eq Prototype
  end

  it "should parse a list of expressions as a Prototype" do
      (res as Prototype).name.should eq "stuff"
  end

  it "should parse args for an expression" do
    expectations = ["a","b","c"]
    args = (res as Prototype).args as Array
    expectations.each_with_index do |arg, index|
      (args[index] as Variable).name.not_nil!.should eq expectations[index]
    end
  end

  it "should parse a list of expressions as a FunBody" do
      (res as Prototype).body.class.should eq FunBody
  end

  it "should capture 3 expressions" do
      (((res as Prototype).body as FunBody).expressions as Array).size.should eq 4
  end

  it "should produce a FunBody with working Expressions" do
    exps = ((res as Prototype).body as FunBody).expressions as Array
    expectations = [12, 1, 1748]
    exps.each_with_index do |exp, index|
      if exp.responds_to? :eval
        v = exp.eval
        if v.responds_to? :value
          v.value.should eq expectations[index]
        end
      else
        fail("bla")
      end
    end
  end

  it "should parse nested function just as any expression" do
    exps = ((res as Prototype).body as FunBody).expressions as Array
    exps[3].class.should eq AArray
    ((exps[3] as AArray).members as Array).first.class.should eq Prototype
    ((((exps[3] as AArray).members as Array).first as Prototype)
      .args as Array).map{|v| (v as Variable).name}.should eq ["x","y"]
  end
end

describe "Kazoo::Language" do
  tuples = [
    {"\"[y(y(7))]\"", "\"[y(y(7))]\""},
    {"1 + 1", "2"},
    {"1 + 1;", "2"},
    {"1 + 1;23", "23"},
    {"1 + 1;23;", "23"},
    {"[1+1, 2*2, true]", "[ 2, 4, true ]"},
    {"fun(x)
        x + 2
      end", "Function (x)"},
    {"def add_two(x)
        x + 2
      end", "Function add_two(x)"},
    {"def add_two(x)
          x + 2
        end
      add_two(2)", "4"},
    {"add = fun(x)
          x + 2
        end
      add(2)", "4"},
    {"addX = fun(x)
          fun(y)
            x + y
          end
        end
      add10 = addX(10)
      add10(10)", "20"},
    {"x = fun(x)
        fun(y)
          x * y
        end
      end
      y = x(7)
      [y(y(7))]", "[ 343 ]"},
    {"send = fun(f,p)
        f(p)
      end
      add2 = fun(x)
        x + 2
      end
      send(add2,5)", "7"},
    {"hash = { a: 1,
               c: true,
               d: fun(x)
                    x + 1
                  end }
      hash", "{
  a: 1,
  c: true,
  d: Function (x)
}"}
  ]
  scope = Kazoo::Scope(Expression).new

  tuples.each do |tuple|
    program, result = tuple

    it "should parse >> #{program} << and evaluate to #{result}" do
      lexer = Kazoo::Lexer
      parser = Kazoo::Parser
      tokens = lexer.lex(program)
      res = parser.parse(tokens, {:accept => :first}) as Expression
      res.eval_scope(scope).to_s.should eq result
    end

  end
end
