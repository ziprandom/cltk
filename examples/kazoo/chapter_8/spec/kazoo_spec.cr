require "spec";
require "../klexer"
require "../kparser"
require "../kcontractor"

def run(program, dump = false)
  (contractor = Kazoo::Contractor.new).execute(
    contractor.optimize(
    contractor.add(
    Kazoo::Parser.parse(
    Kazoo::Lexer.lex(program))).tap {|ir| ir.dump if dump == true }
  )).to_f32
end

describe "CLTK::Kazoo" do
  it "parses, translates and evaluates very simple statement" do
    result = run("13;")
    result.should eq 13
  end

  it "parses, translates and evaluates simple statement" do
    result = run("13 + 12;")
    result.should eq 25
  end

  it "parses, translates and evaluates a list of statements sequentially" do

    result = run("a = 12; a = a + a; b = 3;  a * 3 + b;");
    result.should eq 75
  end

  it "parses, translates and evaluates a conditional branch" do

    result = run("if 0 then 32 else 64;");
    result.should eq 32
  end

  it "parses, translates and evaluates a nested if statement" do

    result = run("if 1 == 2 then if 1 then 10 else 32 else 64;")
    result.should eq 32
  end

  it "parses, translates and evaluates a for loop" do

    result = run("a = 0; for x = 1, x < 10, 1 in a = a + (x * 5) end; a;");
    result.should eq 225
  end

  it "parses, translates and evaluates a for loop" do

    result = run("a = 0; for x = 1, x < 10, 1 in a = a + (x * 5) end; a;")
    result.should eq 225
  end

  it "parses, translates and evaluates a function definition" do
    result = run("def multi(x,y) x * y end; multi(10,5);")
    result.should eq 50
  end

end
