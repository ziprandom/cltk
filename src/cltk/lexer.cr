# Standard Library
require "string_scanner"

# Crystal Language Toolkit
require "./token"
require "./streamposition"
require "./lexer/environment"

def yield_with(env)
  with env yield
end

class StringScanner
  getter :last_match
end

module CLTK

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

    # @return [String] String representation of the error.
    def to_s
      "#{super()}: #{@remainder}"
    end
  end


  # The Lexer class may be sub-classed to produce new lexers.  These lexers
  # have a lot of features, and are described in the main documentation.
  abstract class Lexer

    # @return [Environment] Environment used by an instantiated lexer.
    getter :env

    {% if !@type.has_constant?("BlockReturn") %}
      alias BlockReturn = Tuple(Symbol?, TokenValue)
    {% end %}

    macro inherited
      class Environment < CLTK::Lexer::Environment; end
      alias ProcType = Proc(String, {{@type}}::Environment, BlockReturn?)
      alias FlagsType = Array(Symbol)
      alias RuleType = Tuple(FlagsType, ProcType)

      @@env = {{@type.id}}::Environment
      @@match_type = :longest
      @@regexps = Hash(Tuple(Symbol, FlagsType), Regex?).new
      @@cached_rules_for_state_and_flags = Hash(Tuple(Symbol, FlagsType), Hash(Regex, ProcType)).new
      @@cached_callbacks_for_state = Hash(Symbol, Hash(String, RuleType)).new
      @@callbacks = Hash(Symbol, Hash(String, RuleType)).new
      @@callbacks[:ALL] = Hash(String, RuleType).new
      @@rules = Hash(Symbol, Hash(Regex, RuleType)).new
      @@rules[:ALL] = Hash(Regex, RuleType).new
      @@start_state = :default

      getter :start_state

      def initialize
        @env = {{@type.id}}::Environment.new(@@start_state)
      end

      def lex(string, file_name = nil)
        self.class.lex(string, file_name, @env)
      end

      # Lex *string*, using *env* as the environment.  This method will
      # return the array of tokens generated by the lexer with a token
      # of type EOS (End of Stream) appended to the end.
      #
      # @param [String]		string	String to be lexed.
      # @param [String]		file_name	File name used for recording token positions.
      # @param [Environment]	env		Lexing environment.
      #
      # @return [Array<Token>]
      def self.lex(string, file_name = nil, env = @@env.new(@@start_state))

        # Offset from start of stream.
        stream_offset = 0

        # Offset from the start of the line.
        line_offset = 0
        line_number = 1

        # Empty token list.
        tokens = Array(CLTK::Token).new

        # The scanner.
        scanner = StringScanner.new(string)

        # Start scanning the input string.
        until scanner.eos?
	  match = nil
	  # If the match_type is set to :longest all of the
	  # rules for the current state need to be scanned
	  # and the longest match returned.  If the
	  # match_type is :first, we only need to scan until
	  # we find a match.
	  all_rules(env.state, env.flags).each do |rx, cb|
            if txt = scanner.check(rx)
	      if !match || match.first.size < txt.size
		match = {txt, rx, cb, scanner.last_match}
		break if @@match_type == :first
	      end
	    end
	  end
	  if match
	    txt, rx, cb, last_match = match
            scanner.offset = scanner.offset + txt.size
	    #txt = scanner.scan(rx).not_nil!
            env.match = last_match
            type, value = cb.call(txt, env) || {nil, nil}

	    if type
	      pos = CLTK::StreamPosition.new(stream_offset, line_number, line_offset, txt.size, file_name)
	      tokens << CLTK::Token.new(type, value, pos)
	    end

	    # Advance our stat counters.
	    stream_offset += txt.size

	    if (newlines = txt.count("\n")) > 0
	      line_number += newlines
	      line_offset = txt.split("\n").last.size
	    else
	      line_offset += txt.size()
	    end
	  else
            raise CLTK::LexingError.new(stream_offset, line_number, line_offset, scanner.rest)
	  end
        end

        return tokens << CLTK::Token.new(:EOS)
      end

      def self.filter_for_flags(flags, rules : Hash(Regex, RuleType))
        rules.compact_map do |rx, flag_cb_tuple|
          if (flag_cb_tuple[0] - flags).any?
            nil
          else
            {rx => flag_cb_tuple[1]}
          end
        end.reduce(Hash(Regex, ProcType).new) {|memo, h| memo.merge(h) }
      end

      def self.all_rules(state, flags)
        # Hash(Tuple(Symbol, FlagsType), ProcType)
        @@cached_rules_for_state_and_flags[{state, flags}] ||=
          begin
            rules_for_state_and_flags = filter_for_flags(flags, @@rules[state])
            if rx = regex(state, flags)
              ({
                 rx  => ->(match : String, env : Environment) {
                   callbacks(state)[match][1].call(match, env)
                 }
               }).merge(
                rules_for_state_and_flags
              )
            else
              rules_for_state_and_flags
            end.merge(
              filter_for_flags(flags, @@rules[:ALL])
            ).as(Hash(Regex, ProcType))
          end
        @@cached_rules_for_state_and_flags[{state, flags}]
      end

      def self.callbacks(state)
        @@cached_callbacks_for_state[state] ||=
          (@@callbacks[state]? || Hash(String, RuleType).new).merge(@@callbacks[:ALL])
        @@cached_callbacks_for_state[state]
      end

      def self.regex(state, flags)
        if @@regexps.has_key?({state, flags})
          @@regexps[{state, flags}]
        else
          @@regexps[{state, flags}] =
            callbacks(state).any? ?
              Regex.union(
              callbacks(state).reject{|_, flag_cb_tuple| (flag_cb_tuple[0] - flags).any?}.keys.sort_by(&.size).reverse
            ) : nil
          @@regexps[{state, flags}]
        end
      end

      def self.match_first
        @@match_type = :first
      end
    end

    macro rule(rule, state = :default, flags = [] of Symbol, &block)
      {% if rule.is_a?(StringLiteral) %}
        @@callbacks[{{state}}] ||= Hash(String, RuleType).new
        @@callbacks[{{state}}][{{rule}}] =
      {% else %}
        @@rules[{{state}}] ||= Hash(Regex, RuleType).new
        @@rules[{{state}}][{{rule}}] =
      {% end %}
        {% if block.is_a?(Nop)%}
          Tuple.new({{flags}}, ProcType.new {nil})
        {% else %}
         Tuple.new({{flags}}, ProcType.new do |{{block.args.first || "_".id}}, env|
          res = yield_with(env.as({{@type}}::Environment)) do
             {{block.body}}
          end
          res.is_a?(BlockReturn) ? res : {res, nil}
        end)
        {% end %}
    end

  end
end
