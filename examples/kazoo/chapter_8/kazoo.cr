# Author:		Chris Wailes <chris.wailes@gmail.com>
# Project: 	Ruby Language Toolkit
# Date:		2011/05/09
# Description:	This file is the driver for the Kazoo tutorial.

# Tutorial Files
require "./klexer"
require "./kparser"
require "./llvm_binding_extensions"
require "./kcontractor"

# Load the Kazoo library.
# RLTK::CG::Support.load_library("./libkazoo.so")

# Create our JIT compiler.
jit = Kazoo::Contractor.new
#jit = Kazoo::JIT.new

loop do
  print("Kazoo > ")
  line = " "

  while line[-1..-1] != ";"
    line += " " unless line.empty?
    line += (gets || "").chomp
  end

  if line == "quit;" || line == "exit;"
    break
  end

  begin
    lex = Kazoo::Lexer.lex(line)
    ast = Kazoo::Parser.parse(lex)
    if ast.is_a?(CLTK::ASTNode)
      ir = jit.add(ast)
      #ir = jit.optimize(ir)
      result = jit.execute(ir).to_f64
      puts "=> #{result} (#{result.class})"
    end

  rescue e : CLTK::LexingError
  rescue e : CLTK::NotInLanguage
    puts "Line was not in language."
  rescue e : Exception
    puts e.message
    puts
  end
end
