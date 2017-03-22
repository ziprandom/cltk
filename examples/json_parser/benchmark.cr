require "benchmark"
require "json"
require "../../src/cltk/macros"
require "../../src/cltk/parser/parse"
require "../../src/cltk/parser/type"
require "../../src/cltk/parser/parser"
require "./json_parser"

insert_output_of do
  require "../../src/cltk/parser/tupelize"
  require "./json_parser"
  JSON_PARSE::Parser.tupelize :JSON_PARSER_PRECOMPILED
end

JSON_TEXT = File.read("./example.json")

Benchmark.ips do |x|
  x.report("Stdlib JSON") { JSON.parse(JSON_TEXT)  }
  x.report("CLTK::JSON") do
    JSON_PARSE::Parser.parse(
      JSON_PARSE::Lexer.lex(JSON_TEXT)
    )
  end
  x.report("CLTK::JSON - Precompiled") do
    JSON_PARSER_PRECOMPILED.parse(
      JSON_PARSE::Lexer.lex(JSON_TEXT)
    )
  end
end
