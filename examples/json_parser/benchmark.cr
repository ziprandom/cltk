require "benchmark"
require "json"
require "../../src/cltk/macros"
require "./json_parser"

JSON_TEXT = File.read("./example.json")

Benchmark.ips do |x|
  x.report("Stdlib JSON") { JSON.parse(JSON_TEXT)  }
  x.report("CLTK::JSON") do
    JSON_PARSE::Parser.parse(
      JSON_PARSE::Lexer.lex(JSON_TEXT)
    )
  end
end
