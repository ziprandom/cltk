require "crystal-dfa"
require "./lexer/exceptions"
module CLTK
  #
  # A Lexer Class based on [crystal-dfa](https://github.com/ziprandom/crystal-dfa) the Crystal RegExp implementation
  # of a Thompson NFA based DFA. Runs faster than (PCRE based) Regex implementations. Define a Lexer like:
  # ```
  # class CalcLexer < Scanner
  #   # set a delimiter to strip the
  #   # string before lexing for increased
  #   # performance. defaults to "\n" which
  #   # is fine for this example.
  #
  #   # self.pre_delimiter=nil # no string splitting
  #   # before lexing
  #
  #   # ignore space & newline
  #   rule(/[\n\s]/)
  #
  #   # operators are keywords, so we use
  #   # stringrules to have one single
  #   # dfa matching
  #   rule("+")                { {:PLS} }
  #   rule("-")                { {:SUB} }
  #   rule("*")                { {:MUL} }
  #   rule("/")                { {:DIV} }
  #
  #   # ints and floats need to be matched with
  #   # regular expressions (doesn't use Regex but
  #   # DFA::RegExp)
  #   rule(/\d+\.\d+/)         { |s| {:FLOAT, s} }
  #   rule(/\d+/)              { |s| {:INT,   s} }
  #
  #   # upon sighting of a '#' (and optionally trailing ' ')
  #   # we go into :comment state and don't leave until we
  #   # find a '\n'
  #   rule(/#\s*/)             {     push_state(:comment) }
  #   rule(/[^\n]+/, :comment) { |t| {:COMMENT, t}        }
  #   rule("\n", :comment)     {     pop_state            }
  #
  #   # calculate the dfa's for the string-rules
  #   # will be called upon first `lex` call if
  #   # not invoked here
  #   finalize
  # end
  #
  # source = <<-source
  # #
  # # a simple calculation
  # #
  #
  # 4 + 4  # the first addition
  # - 3.14 # a substraction
  # * 3
  #
  # source
  #
  # pp CalcLexer.lex(source).tokens   # => [{:COMMENT, "a simple calculation"},
  #                                   #     {:INT, "4"},
  #                                   #     {:PLS},
  #                                   #     {:INT, "4"},
  #                                   #     {:COMMENT,
  #                                   #      "the first addition"},
  #                                   #     {:SUB},
  #                                   #     {:FLOAT,
  #                                   #      "3.14"},
  #                                   #     {:COMMENT,
  #                                   #      "a substraction"},
  #                                   #     {:MUL},
  #                                   #     {:INT, "3"}]
  # ```
  #
  abstract class Scanner

    alias Token = Tuple(Symbol, TokenValue) | Tuple(Symbol)
    alias StreamPosition = NamedTuple(position: Int32, size: Int32)

    alias ProcType = Proc(String, Environment, Token?)

    @@strings = Hash(Symbol, Hash(String, ProcType?)).new
    @@callbacks = Array(ProcType).new
    @@rx = Hash(Symbol, Array({DFA::DFA::DState, Int32?})).new
    @@is_finalized = false

    # In order to speed up lexing, the string might be split
    # in single lines and therefore fed to the dfas in smaller
    # chunks. this is enabled by default, but can be disabled
    # with this class setter
    class_property split_lines : Bool = true

    #
    # Used to keep track of the Lexing State. Used
    # as context for token callback yields and
    # gets returned by the lexer
    #
    class Environment
      @states    = [:default]
      @tokens    = Array(Token).new
      @positions = Array(StreamPosition).new
      @offset    = 0
      # the lexed tokens
      getter :tokens
      # positions for lexed tokens
      getter :positions
      # current lexing position
      getter :offset
      # get the currently active lexing state
      def state; @states.last; end
      # push a new lexing state to the stack
      def push_state(state : Symbol); @states.push(state); nil; end
      # pop the last state from the stack
      def pop_state; @states.pop; nil; end

      # :nodoc:
      def yield_with_self
        with self yield
      end

      # returns an array of all lexed
      # tokens and their positions
      def tokens_with_positions
        @tokens.zip(@positions)
      end

      # add a token at the current position
      # doesn't advance the @offset index
      def add_token(token : Token, size : Int32)
        @tokens << token
        @positions << StreamPosition.new(position: @offset, size: size)
      end

      # advance the offset by count
      def advance_position(count : Int32)
        @offset += count
      end

      # yield the given block for each token and it's
      # position and returns the results in an array
      def map
        i = -1
        @tokens.map_with_index do |token, index|
          with self yield token, @positions[index]
        end
      end
    end

    #
    # wrap lex and #lex to return the Tokens
    # we expect from a CLTK::Lexer
    #
    module LexerCompatibility
      macro extended
        def self.lex(string)
          super(string).map do |token, position|
            pos = CLTK::StreamPosition.new(
              position[:position], 0,
              position[:position], position[:size]
            )
	    CLTK::Token.new(token[0], token[1]?, pos)
          end << CLTK::Token.new(:EOS)
        end

        def lex(string)
          self.class.lex(string)
        end
      end
    end


    #
    # Defines a lexing rule. The expression can either
    # be a string or a `DFA::RegExp` compatible expression.
    # State indicates a Lexer State in which this Rule should
    # be applied. String Expressions for the same state get
    # combined in one alternating (..|..|..) DFA for faster
    # recognition. String Expressions should be used for
    # special keywords and symbols like: def, true or ":"
    #
    macro rule(expression, state = :default, &block)
      {% if expression.is_a? RegexLiteral %}
        rex_rule({{expression.source}}, {{state}}) {{block}}
      {% else %}
        string_rule({{expression}}, {{state}}) {{block}}
      {% end %}
    end

    # finalize the Lexer by creating dfas for the provided
    # string rules for fast keyword matching
    def self.finalize
      @@strings.each do |state, hash|
        litdfa = DFA::RegExp.new(hash.map {|k, _| Regex.escape(k) }.join("|")).dfa
        @@rx[state] ||= Array({DFA::DFA::DState, Int32?}).new
        @@rx[state] << ({ litdfa, @@callbacks.size+1 })
        cb = ProcType.new { |string, env| hash[string].try &.call(string, env)}
        @@callbacks.unshift cb
      end
      @@is_finalized =  true
    end

    # lexes a string by continously matching the dfas
    # against the string, yielding the callbacks with
    # an instance of Environment
    def self.lex(string : String) : Environment
      finalize unless @@is_finalized
      env = Environment.new
      @@split_lines ?
        string.lines(false).each do |line|
          lex_string(line, env)
        end :
        lex_string(string, env)
      env
    end

    # continously match the string against the dfas
    # calling the returned callbacks with the matches
    # to construct the Token Values
    private def self.lex_string(string, env)
      i = 0
      while (i <= string.size-1)
        s = string[i..-1]
        size, cbindex = match(s, env)
        cbindex && (value = @@callbacks[-cbindex].try(&.call(s[0, size], env)) ) && env.add_token(value, size)
        env.advance_position(size)
        i += size
      end
    end

    # runs the set of dfas in @@rx against the string
    # and returns the index and last position of the
    # dfa automaton that achieved the longes match
    private def self.match(string : String, env)
      match_end = nil
      dfas = @@rx[env.state]
      string.each_char_with_index do |c, i|
        break unless dfas.size > 0
        dfas = dfas.compact_map do |d|
          if dd = d[0].next.find {|x| x[0][0] <= c.ord <= x[0][1] }.try(&.[1])
            match_end = {i+1, d[1]} if dd.accept
            {dd, d[1]}
          end
        end
      end
      unless match_end
        raise CLTK::Lexer::Exceptions::LexingError.new(env.offset, 0, 0, string)
      end
      match_end
    end

    # macro for creating a RegExp Rule
    private macro rex_rule(rule, state, &block)
      {% unless block.is_a?(Nop)%}
        @@callbacks.unshift(block_to_proc {{block}})
      {% end %}
        @@rx[{{state}}] ||= Array({DFA::DFA::DState, Int32?}).new
        @@rx[{{state}}].unshift ({ DFA::RegExp.new({{rule}}).dfa,
                        {% if block.is_a?(Nop)%}
                          nil
                        {% else %}
                        @@callbacks.size
                        {% end %}
                      })
    end

    # macro for adding a String to the states String Matching DFA
    private macro string_rule(string, state, &block)
      @@strings[{{state}}] ||= Hash(String, ProcType?).new
      @@strings[{{state}}][{{string}}] = {% if block.is_a?(Nop)%}
         nil
      {% else %}
        block_to_proc {{block}}
      {% end %}
    end

    # wrap the given block to be yielded in an Environment
    private macro block_to_proc(&block)
      {%unless block.is_a? Nop %}
        ProcType.new do |{{block.args.first || "_".id }}, env|
          env.yield_with_self do
            {{block.body}}
          end
        end
      {% else %}
        nil
      {% end %}
    end

  end
end
