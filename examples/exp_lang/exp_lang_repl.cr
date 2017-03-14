require "readline"
require "./xlexer"
require "./xast"
require "./xscope"
require "../../src/cltk/macros"
require "../../src/cltk/parser/type"
require "../../src/cltk/parser/parse"

insert_output_of() do
  require "../../src/cltk/parser/tupelize"
  require "./xparser"
  EXP_LANG::Parser.tupelize
end

lexer  = EXP_LANG::Lexer
parser = EXP_LANG::Parser
scope  = EXP_LANG::Scope(Expression).new

input = ""

puts "\n\n" +
     "  welcome to the example repl, exit with: 'exit'  \n" +
     "--------------------------------------------------\n\n"

while true
  begin

    # read input
    input = Readline.readline(":: ", true) || ""

    # exit on exit

    exit if input == "exit"

    # lex input
    tokens = lexer.lex(input)

    # parse lexed tokens
    res = parser.parse(tokens, {accept: :first}).as(CLTK::ASTNode)

    # evaluate the result with a given scope
    # (scope my be altered by the expression)
    evaluated =  res.eval_scope(scope).to_s

    # output result of evaluation
    puts evaluated

  rescue e: CLTK::LexingError
    show_lexing_error(e, input)
  rescue e: CLTK::NotInLanguage
    show_syntax_error(e,input)
  rescue e
    puts e
  end
end

def show_lexing_error(e, input)
  puts "Lexing error at:\n\n"
  puts "    " + input.split("\n")[e.line_number-1]
  puts "    " + e.line_offset.times().map { "-" }.join + "^"
  puts e
end

def show_syntax_error(e,input)
    pos = e.current.position
    if pos
      puts "Syntax error at:"
      puts "    " + input.split("\n")[pos.line_number-1]
      puts "    " + pos.line_offset.times().map { "-" }.join + "^"
    else
      puts "invalid input: #{input}"
    end
end
