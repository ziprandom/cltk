# coding: utf-8
require "spec"
require "../json_parser"

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
  end.as(JsonExpression)
end

describe("json_lexer") do

  it "parses a number" do
    result = parse_json("23").as(JsonNumber)
    JSON_PARSE::JsonOutputer.new(result).print.should eq("23")
  end

  it "parses a negative float number" do
    result = parse_json("-23.3").as(JsonNumber)
    result.number.should eq(-23.3)
  end

  it "parses a bool" do
    result = parse_json("false").as(JsonBool)
    result.bool.should eq(false)
    result = parse_json("true").as(JsonBool)
    result.bool.should eq(true)
  end

  it "parses a null" do
    result = parse_json("null").as(JsonNull)
  end

  it "parses a string" do
    result = parse_json("\"hallo json\"")
    JSON_PARSE::JsonOutputer.new(result).print.should eq("\"hallo json\"")
  end

  it "parses an array" do
    result = parse_json("[12              ,
                          123,
                          \"json\"     ,null          ]")
    JSON_PARSE::JsonOutputer.new(result).print.should eq("[\n  12,\n  123,\n  \"json\",\n  null\n]")
  end

  it "parses an object" do
    result = parse_json("{\"name\": \"peter\", \"age\": 23, \"profession\": \"programmer\"}")
    string_result = JSON_PARSE::JsonOutputer.new(result).print
    string_result.should eq("{\n  \"name\": \"peter\",\n  \"age\": 23,\n  \"profession\": \"programmer\"\n}")
  end

  it "parses a complex object" do
    json_text = "{
      \"username\": \"walter\",
      \"friends\": [
          \"granny\",
          \"sophia\",
          \"maud\"
      ],
      \"address\": {
          \"street\": \"neverstreet\",
          \"number\": 12,
          \"city\": \"nowhere\"
      }
    }"

    result = parse_json(json_text)
    string_result = JSON_PARSE::JsonOutputer.new(result).print
    string_result.should eq("{\n  \"username\": \"walter\",\n  \"friends\": [\n    \"granny\",\n    \"sophia\",\n    \"maud\"\n  ],\n  \"address\": {\n    \"street\": \"neverstreet\",\n    \"number\": 12,\n    \"city\": \"nowhere\"\n  }\n}")
  end

end
