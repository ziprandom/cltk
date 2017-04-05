module CLTK
  alias TokenValue = (String|Int32)?
end
require "../../src/cltk"

module BrainFuck

  #
  # Lexer
  #
  class Lexer < CLTK::Lexer
    rule(/>/)  { :PTRRIGHT }
    rule(/</)  { :PTRLEFT  }
    rule(/\+/) { :INC      }
    rule(/-/)  { :DEC      }
    rule(/\./) { :PUT      }
    rule(/,/)  { :GET      }
    rule(/\[/) { :LBRACKET }
    rule(/\]/) { :RBRACKET }
    # ignore the rest
    rule(/\n/)
    rule(/./)
  end

  #
  # Parser
  #
  class Parser < CLTK::Parser
    production(:program, "op+") { |ops| Program.new(operations: ops) }

    production(:op) do
      clause("PTRRIGHT")                      { |_| PtrRight.new  }
      clause("PTRLEFT")                       { |_| PtrLeft.new   }
      clause("INC")                           { |_| Increment.new }
      clause("DEC")                           { |_| Decrement.new }
      clause("PUT")                           { |_| Put.new       }
      clause("GET")                           { |_| Get.new       }
      clause("LBRACKET op+ RBRACKET") { |_, ops, _| Loop.new(operations: ops) }
    end

    finalize
  end

  #
  # ASTNode Definitions
  #
  class Operation < CLTK::ASTNode; end
  class PtrRight < Operation; end
  class PtrLeft < Operation; end
  class Increment < Operation; end
  class Decrement < Operation; end
  class Put < Operation; end
  class Get < Operation; end

  class Loop < Operation
    values({
      operations: Array(Operation),
    })
  end

  class Program < CLTK::ASTNode
    values({
      operations: Array(Operation),
    })
  end

  #
  # Interpreter
  #
  class Interpreter
    def initialize(@program : Program)
      @tape = Array(Int32?).new(30000) { |index| nil }
      @pointer = 0
      @output = [] of Int32
    end

    def exec_cmd(cmd)
      case cmd
      when PtrRight
        @pointer += 1
      when PtrLeft
        @pointer -= 1
      when Increment
        @tape[@pointer] = (@tape[@pointer] || 0) + 1
      when Decrement
        @tape[@pointer] = (@tape[@pointer] || 0) - 1
      when Put
        if !(curr = @tape[@pointer]).nil?
          @output << curr
        end
      when Get
        @tape[@pointer] = (gets(1) || "").to_i
      when Loop
        while @tape[@pointer] != 0
          cmd.operations.each do |op|
            exec_cmd op
          end
        end
      end
    end

    def run
      @program.operations.each { |op|
        exec_cmd op
      }
      @output
    end
  end
end
