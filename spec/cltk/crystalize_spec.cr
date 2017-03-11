require "spec"
require "../../src/cltk/lexers/calculator"
require "../../src/cltk/parser/parser"
require "../../src/cltk/parser/type"
require "../../../src/cltk/macros"

# insert_output_of() do
#   require "../../src/cltk/parser"
#
#   class AmbiguousParserExample < CLTK::Parser
#     production(:e) do
#       clause("NUM") {|n| n.as(Int32)}
#
#       clause("e PLS e") { |e0, op, e1 | e0.as(Int32) + e1.as(Int32) }
#       clause("e SUB e") { |e0, op, e1 | e0.as(Int32) - e1.as(Int32) }
#       clause("e MUL e") { |e0, op, e1 | e0.as(Int32) * e1.as(Int32) }
#       clause("e DIV e") { |e0, op, e1 | e0.as(Int32) / e1.as(Int32) }
#       nil
#
#     end
#
#     finalize
#   end
#
#   AmbiguousParserExample.crystalize()
# end

pending "CLTK::Parser::Parser.crystalize" do
  it "test_ambiguous_grammar" do
    actual = AmbiguousParserExample.not_nil!.parse(
      CLTK::Lexers::Calculator.lex("1 + 2 * 3"), {accept: :all}
    )
    actual.should eq [9,7]
  end
end
