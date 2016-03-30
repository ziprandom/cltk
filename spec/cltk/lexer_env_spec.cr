#
# as there is a bug with inheritance and procs
# (https://github.com/manastech/crystal/issues/729)
# we have to use "monkey-patching" here:
#
# class CLTK::Lexer::Environment
#   @value = -1
#   def next_value
#     if @value
#       @value += 1
#       @value as Int32
#     else
#       @value = -1
#     end
#   end
# end
#
#
# class ENVLexer < CLTK::Lexer
#
#   rule(/a/) do |txt, env|
#     puts env
#     {:A, env.next_value}
#   end
#
# end
describe "CLTK::Lexer" do

  pending "can instantiate a lexer with a custom environment" do
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
end
