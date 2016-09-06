require "./json_parser"

def parse_json(json_text)
  begin
    tokens = JSON_PARSE::Lexer.lex(json_text)
    JSON_PARSE::Parser.parse(
      tokens
    )
  # initializations
  # lex, parse, interpret
  rescue e: CLTK::LexingError
    puts "Lexing Error"
  rescue e: CLTK::NotInLanguage
    puts "Not In Language"
  rescue e
    puts "an error occured"
  end.as(JSON_PARSE::JsonExpression)
end

piped_in = IO::ARGF.new(ARGV, STDIN).gets_to_end
result = parse_json(piped_in)
puts JSON_PARSE::JsonOutputer.new(result).print
