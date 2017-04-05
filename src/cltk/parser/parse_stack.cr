module CLTK

  def self.is_terminal?(sym)
    sym && (s = sym.to_s) == s.upcase
  end

  def self.is_nonterminal?(sym)
    sym && sym.to_s == sym.to_s.downcase
  end

  class Parser
    # The ParseStack class is used by a Parser to keep track of state
    # during parsing.
    class ParseStack
    # @return [Integer] ID of this parse stack.
    getter :id

    # @return [Array<Object>] Array of objects produced by {Reduce} actions.
    getter :output_stack

    # @return [Array<Integer>] Array of states used when performing {Reduce} actions.
    getter :state_stack

    @cbuffer = Array(Int32).new
    @output_stack = Array(CLTK::Type).new

    # Instantiate a new ParserStack object.
    #
    # @param [Integer]                id           ID for this parse stack.  Used by GLR algorithm.
    # @param [Array<Object>]          ostack       Output stack.  Holds results of {Reduce} and {Shift} actions.
    # @param [Array<Integer>]         sstack       State stack.  Holds states that have been shifted due to {Shift} actions.
    # @param [Array<Integer>]         nstack       Node stack.  Holds dot language IDs for nodes in the parse tree.
    # @param [Array<Array<Integer>>]  connections  Integer pairs representing edges in the parse tree.
    # @param [Array<Symbol>]          labels       Labels for nodes in the parse tree.
    # @param [Array<StreamPosition>]  positions    Position data for symbols that have been shifted.
    def initialize(@id : Int32, @output_stack = [] of Type, @state_stack = [0] of Int32, @node_stack = [] of Int32,
                                                                                                             @connections = [] of {Int32, Int32}, @labels = [] of String, @positions = [] of StreamPosition)

    end

    # Branch this stack, effectively creating a new copy of its
    # internal state.
    #
    # @param [Integer] new_id ID for the new ParseStack.
    #
    # @return [ParseStack]
    def branch(new_id)
      ParseStack.new(new_id, @output_stack.clone, @state_stack.dup,
		     @node_stack.dup, @connections.dup, @labels.dup, @positions.dup)
    end

    # @return [StreamPosition] Position data for the last symbol on the stack.
    def position
      if @positions.empty?
	StreamPosition.new
      else
	@positions.last.dup
      end
    end

    # Push new state and other information onto the stack.
    #
    # @param [Integer]			state	ID of the shifted state.
    # @param [Object]			o		Value of Token that caused the shift.
    # @param [Symbol]			node0	Label for node in parse tree.
    # @param [StreamPosition]	position	Position token that got shifted.
    #
    # @return [void]
    def push(state, o, node0, position)
      @state_stack << state
      if o.is_a? Array
        @output_stack = @output_stack + [o.map{ |e| e.as(Type) }.as(Type)]
      elsif o.is_a? Type
        @output_stack << o
      else
        @output_stack << o.as(Type)
      end

      @node_stack	<< @labels.size
      @labels		<< if CLTK.is_terminal?(node0) && o
      node0.to_s + "(#{o})"
    else
      node0.to_s
    end.as(String)
    @positions << position.not_nil!

    if CLTK.is_nonterminal?(node0)
      @cbuffer.each do |node1|
	@connections << {@labels.size - 1, node1}
      end
    end
  end

  # Pop some number of objects off of the inside stacks.
  #
  # @param [Integer] n Number of object to pop off the stack.
  #
  # @return [Array(Object, StreamPosition)] Values popped from the output and positions stacks.
  def pop(n = 1)
    @state_stack.pop(n)
    # Pop the node stack so that the proper edges can be added
    # when the production's left-hand side non-terminal is
    # pushed onto the stack.
    @cbuffer = @node_stack.pop(n)
    {@output_stack.pop(n), @positions.pop(n)}
  end

  # Fetch the result stored in this ParseStack.  If there is more
  # than one object left on the output stack there is an error.
  #
  # @return [Object] The end result of this parse stack.
  def result
    if @output_stack.size == 1
      return @output_stack.last
    else
      raise InternalParserException.new "The parsing stack should have 1 element on the output stack, not #{@output_stack.size}."
    end
  end

  # @return [Integer] Current state of this ParseStack.
  def state
    @state_stack.last
  end

  # @return [String] Representation of the parse tree in the DOT langauge.
  def tree
    tree  = "digraph tree#{@id} {\n"

    @labels.each_with_index do |label, i|
      tree += "\tnode#{i} [label=\"#{label}\""

      if CLTK.is_terminal?(label)
	tree += " shape=box"
      end

      tree += "];\n"
    end

    tree += "\n"

    @connections.each do |from, to|
      tree += "\tnode#{from} -> node#{to};\n"
    end

    tree += "}"
  end

end

end
end
