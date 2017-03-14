require "../../src/cltk/lexers/calculator"
require "../../src/cltk/parser/parse"
require "../../src/cltk/parser/type"
require "../../src/cltk/parser/environment"
require "../../src/cltk/macros"
require "spec"

insert_output_of() do
  require "../../src/cltk/parser"
  require "../../src/cltk/parser/tupelize"

  class MiniParser < CLTK::Parser
    production(:e) do
      clause("NUM") {|n| n.as(Int32)}

      clause("e PLS e") { |e0, op, e1 | e0.as(Int32) + e1.as(Int32) }
      clause("e SUB e") { |e0, op, e1 | e0.as(Int32) - e1.as(Int32) }
      clause("e MUL e") { |e0, op, e1 | e0.as(Int32) * e1.as(Int32) }
      clause("e DIV e") { |e0, op, e1 | e0.as(Int32) / e1.as(Int32) }
    end
    finalize
    tupelize
  end
end


#describe "parsing with parser data struct" do
  it "works" do
    tokens = CLTK::Lexers::Calculator.lex("1 + 2 * 3")
    actual = MiniParser.parse(tokens, {accept: :all})
    actual.should eq [9, 7]
  end
#end
#
