# coding: utf-8
require "../../src/cltk/lexer"
require "../../src/cltk/ast"
require "../../src/cltk/parser"
require "../kazoo/chapter_8/visitor"

module JSON_PARSE

  # The Lexer

  class Lexer < CLTK::Lexer
    # Skip whitespace.
    rule(/\n/)
    rule(/\s/)

    rule(/:/)	  { :COLON   }
    rule(/\[/)	  { :LBRACK  }
    rule(/\]/)	  { :RBRACK  }
    rule(/\{/)	  { :LCBRACK }
    rule(/\}/)	  { :RCBRACK }
    rule(/,/)	  { :COMMA   }

    rule(/true/)  { {:BOOL, 0} }
    rule(/false/) { {:BOOL, 1} }

    # String with included quoted strings
    rule(/"(?:[^"\\]|\\.)*"/) { |t| {:STRING, t[1...-1]}}

    # Numeric rules.
    rule(/\-?\d+/)            { |t| {:NUMBER, t.to_f} }
    rule(/\-?\d+\.\d+/)	      { |t| {:NUMBER, t.to_f} }
  end

  # The AST Nodes

  class JsonExpression < CLTK::ASTNode;  end

  class JsonBool < JsonExpression
    values({
             bool: Bool
           })
  end

  class JsonNumber < JsonExpression
    values({
             number: Float64
           })
  end

  class JsonString < JsonExpression
    values({
             string: String
           })
  end

  class JsonObject < JsonExpression
    values({
             hash: Hash(String, JsonExpression),
           })
  end

  class JsonArray < JsonExpression
    children({
               elements: Array(JsonExpression),
             })
  end

  # The Parser

  class Parser < CLTK::Parser

    production(:json) do
      clause(:expression) { |e| e }
    end

    production(:expression) do
      clause(:json_object) { |o| o }
      clause(:json_array)  { |a| a }
      clause(:json_string) { |s| s }
      clause(:json_number) { |n| n }
      clause(:json_bool)   { |b| b }
    end

    production(:json_number) do
      clause(:NUMBER)	{ |n| JsonNumber.new(n as Float64); }
    end

    production(:json_string) do
      clause(:STRING)	{ |s| JsonString.new(s) }
    end

    production(:json_bool) do
      clause(:BOOL)	{ |b| JsonBool.new( b == 0 ? true : false) }
    end

    production(:json_array) do
      clause("LBRACK array_elements RBRACK") do |_, elements, _|
        JsonArray.new(elements)
      end
    end

    build_list_production(:array_elements, :expression, :COMMA)

    production(:json_object) do
      clause("LCBRACK hash_pairs RCBRACK") do |_, hash_pairs, _|
        hash = (hash_pairs as Array).reduce(Hash(String, JsonExpression).new) do |hash, pair|
          pair = pair as Array
          key = pair[0] as JsonString
          value = pair[1] as JsonExpression
          hash[key.string.not_nil!] = value
          hash
        end
        JsonObject.new(hash)
      end
    end

    build_list_production(:hash_pairs, :hash_pair, :COMMA)

    production(:hash_pair) do
      clause("json_string COLON expression") {|key, _, value| [key, value] }
    end

    finalize
  end

  #
  # A Visitor Class to keep
  # track of the indentation
  # level for nested structures
  #

  class JsonSerializer
    include CLTK::Visitor(Int32)

    def initialize(@node : JsonExpression)
      @env = -2
    end

    def print
      visit @node
    end

    def visit(node, skip_first_line = false)
      result = ""
      @env = @env.tap do |env|
        @env = 2 + env as Int32
        result = indent_text(
          (wrapped_visit(node) as String),
          @env, skip_first_line
        )
      end
      result
    end

    def indent_text(text, level, skip_first_line)
      indent = level.times.map{" "}.join
      lines = text.split("\n")
      if skip_first_line == true
        if lines.size > 1
          lines[0] + "\n" + lines[1..-2].map{|line| indent + line }.join("\n") + "\n" + indent[0..-1] + lines.last
        else
          lines[0]
        end
      else
        lines.map{|line| indent + line }.join("\n")
      end
    end

  end

  #
  # Output JSON
  #
  class JsonOutputer < JsonSerializer

    on JsonArray do |array|
      serialized_children = (array.elements as Array).map do |child|
        "  " + (visit child, true) as String
      end.join(",\n")
      "[\n" + serialized_children + "\n]"
    end

    on JsonObject do |object|
      serialized_children = (object.hash as Hash).map do |key, value|
        ("  \"#{key}\": " + visit(value, true) as String)
       end.join(",\n")
      "{\n" + serialized_children + "\n}"
    end

    on JsonBool do |bool|
      bool.bool ? "true" : "false"
    end

    on JsonString do |string|
      "\"" + string.string.not_nil! + "\""
    end

    on JsonNumber do |number|
      number.number.to_s
    end
  end

  #
  # Output XML
  #
  class XmlOutputer < JsonSerializer

    on JsonArray do |array|
      serialized_children = (array.elements as Array).map do |child|
        "  <item>\n" + (visit child) as String + "\n  </item>"
      end.join("\n")
      "<list>\n" + serialized_children + "\n</list>"
    end

    on JsonObject do |object|
      serialized_children = (object.hash as Hash).map do |key, value|
          ("<#{key}>\n" + visit(value) as String + "\n</#{key}>")
      end.join("\n")
      serialized_children as String
    end

    on JsonBool do |bool|
      bool.bool ? "true" : "false"
    end

    on JsonString do |string|
      string.string.not_nil!
    end

    on JsonNumber do |number|
      number.number.to_s
    end
  end

end
