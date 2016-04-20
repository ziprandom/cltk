require "spec"
require "../brainfuck"

struct Number
  def chr
    String.new(1) do |buffer|
      buffer[0] = self.to_u8
      {1,1}
    end
  end
end

describe("brainfuck") do

  # source
  program_text = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

  it "lexes, parses and interprets a brainck program" do

    result = begin

      # initializations
      lexer  = BrainFuck::Lexer
      parser = BrainFuck::Parser
      interpreter = BrainFuck::Interpreter

      # lex, parse, interpret
      tokens = lexer.lex(program_text)
      intermediate_representation = parser.parse(tokens) as BrainFuck::Program
      result = interpreter.new(intermediate_representation).run

      # output map ordinal integers to chars and join
      result.map do |ordinal_number|
        if (ordinal_number)
          ordinal_number.chr
        end
      end.join

    rescue e: CLTK::LexingError
      puts "Lexing Error"
    rescue e: CLTK::NotInLanguage
      puts "Not In Language"
    rescue e
      puts e
    end

    result.should eq("Hello World!\n")
  end

end
