require "./json_parser"
require "benchmark"
require "json"

JSON_TEXT = File.read("./mtg_json.json")

Benchmark.ips do |x|
  x.report("Stdlib JSON") { JSON.parse(JSON_TEXT)  }
  x.report("CLTK::JSON") do
    JSON_PARSE::Parser.parse(
      JSON_PARSE::Lexer.lex(JSON_TEXT)
    )
  end
end
