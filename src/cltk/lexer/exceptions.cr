module CLTK
  abstract class Lexer
             module Exceptions
               # A LexingError exception is raised when an input stream contains a
               # substring that isn't matched by any of a lexer's rules.
               class LexingError < Exception
                 # @return [Integer]
                 getter :stream_offset

                 # @return [Integer]
                 getter :line_number

                 # @return [Integer]
                 getter :line_offset

                 # @return [String]
                 getter :remainder

                 # @param [Integer]	stream_offset	Offset from begnning of string.
                 # @param [Integer]	line_number	Number of newlines encountered so far.
                 # @param [Integer]	line_offset	Offset from beginning of line.
                 # @param [String]	remainder		Rest of the string that couldn't be lexed.
                 def initialize(@stream_offset : Int32, @line_number : Int32, @line_offset : Int32, @remainder : String)
                   super(message)
                   @backtrace = [] of String
                 end
               end
             end
           end
end
