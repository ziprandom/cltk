require "readline"
require "./klexer"
require "./kparser"
require "./kast"
require "./kscope"

lexer = Kazoo::Lexer
parser = Kazoo::Parser
scope = Kazoo::Scope(Expression).new

input = ""

while !(input||"").match /^exit$/
  input = Readline.readline(":: ", true)
  if input.is_a? String
    begin
      tokens = lexer.lex(input)
    rescue e: CLTK::LexingError
      pp(e)
      next
    end
    begin
      res = parser.parse(tokens, {:accept => :first})
      if res.is_a? CLTK::ASTNode
        begin
          puts res.eval_scope(scope).to_s
        rescue e
          puts e
        end
      end
    rescue e: CLTK::NotInLanguage
      pos = e.current.position
      if pos
        puts "Syntax error at:"
        puts input
        puts pos.line_offset.times().map { "-" }.join + "^"
      else
        puts "invalid input: #{input}"
      end
    end
  end
end
