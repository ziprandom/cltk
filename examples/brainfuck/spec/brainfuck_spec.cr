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
  program_text = "# Program Source        # Explanation
                  #                       #
                  #############################################################
                  +++++ +++++             # initialize counter (cell #0) to 10
                  [                       # use loop to set the next four cells
                                          # to 70/100/30/10
                      > +++++ ++          #     add  7 to cell #1
                      > +++++ +++++       #     add 10 to cell #2
                      > +++               #     add  3 to cell #3
                      > +                 #     add  1 to cell #4
                      <<<< -              #     decrement counter (cell #0)
                  ]
                  > ++ .                  # print 'H'
                  > + .                   # print 'e'
                  +++++ ++ .              # print 'l'
                  .                       # print 'l'
                  +++ .                   # print 'o'
                  > ++ .                  # print ' '
                  << +++++ +++++ +++++ .  # print 'W'
                  > .                     # print 'o'
                  +++ .                   # print 'r'
                  ----- - .               # print 'l'
                  ----- --- .             # print 'd'
                  > + .                   # print '!'
                  > .                     # print '\n'"

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
