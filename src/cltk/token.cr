require "./streamposition"
# Description:	This file contains code having to do with tokens.

#######################
# Classes and Modules #
#######################

module CLTK
  # The Token class is used to represent the output of a RLTK::Lexer and the
  # input of a RLTK::Parser.
  {% if !@type.has_constant?("TokenValue") %}
    alias TokenValue = String?
  {% end %}
  class Token
    # @return [Symbol]
    getter :type

    # @return [Symbol]
    getter :value

    # @return [StreamPosition] StreamPosition object associated with this token.
    getter :position

    # Instantiates a new Token object with the values specified.
    #
    # @param [Symbol]				type		A symbol representing the type of this Token.
    # @param [Object, nil]			value	A value associated with this token.
    # @param [StreamPosition, nil]	position	The position of the token in a stream.
    def initialize(
          @type : String|Symbol,
          @value : TokenValue = nil,
          @position : StreamPosition? = nil)
    end

    # Compares one token to another.  This only tests the token's *type*
    # and *value* and not the location of the token in its source.
    #
    # @param [Token] other Another Token to compare to.
    #
    # @return [Boolean]
    def ==(other)
      if other.is_a? Token
	@type == other.type && @value == other.value
      else
        false
      end
    end

    def clone
      self
    end

    # @return [String] String representing the tokens *type* and *value*.
    def to_s
      if value
	"#{@type}(#{@value})"
      else
	@type.to_s
      end
    end
  end
end
