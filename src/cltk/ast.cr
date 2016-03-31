abstract class CLTK::ASTNode

  property :parent

  def root
    parent || self
  end

  {% if @type == CLTK::ASTNode %}
    VALUES_NAMES = [] of Symbol
    VALUES_DEFAULTS = [] of Nil
    CHILDREN_DEFAULTS = Array(CLTK::ASTNode?|Array(CLTK::ASTNode?)).new
    CHILDREN_NAMES = [] of Symbol
    ORDER = :children
    CUSTOM_ORDER = [] of Symbol
  {% else %}
    VALUES_NAMES = ({{@type.superclass}}::VALUES_NAMES as Array(Symbol)).clone
    VALUES_DEFAULTS = {{@type.superclass}}::VALUES_DEFAULTS.clone
    CHILDREN_DEFAULTS = {{@type.superclass}}::CHILDREN_DEFAULTS.clone
    ORDER = {{@type.superclass}}::ORDER.clone
    CUSTOM_ORDER = {{@type.superclass}}::CUSTOM_ORDER.clone
  {% end %}

  class ValueNotSet; end;

  macro order(order)
    {{@type.id}}::ORDER = {{order}}
  end

  macro custom_order(*custom_order)
    CUSTOM_ORDER = {{custom_order}}
  end

  macro values(values_hash)
    {% if @type.superclass == CLTK::ASTNode %}
      VALUES_NAMES = {{values_hash.keys}}
      VALUES_DEFAULTS = Array(({%for type in values_hash.values%}{{type.id}}|{%end%}Nil)).new
    {% else %}
      VALUES_NAMES = ({{@type.superclass}}::VALUES_NAMES as Array(Symbol)) + {{values_hash.keys}}
      VALUES_DEFAULTS =  {{@type.superclass}}::VALUES_DEFAULTS.clone
    {% end %}
    {% for value_name, value_type in values_hash %}
      def {{value_name.id}}
        @{{value_name.id}} || {% if  value_type.id.stringify.starts_with? "Array" %} [] of {{value_type.id.gsub(/^Array\(|\)$/, "")}} {% else %} nil {% end %}
      end

      def {{value_name.id}}=(value : {{value_type}})
        @{{value_name.id}} = value
      end

      def {{value_name.id}}(value : {{value_type}})
        {{value_name.id}}=(value)
      end

      def {{value_name.id}}(value : {{value_type}})
          @{{value_name.id}} = value
      end
    {% end%}

    def values
      super() +
      [{% for value_name, index in values_hash.keys %}self.{{value_name.id}}{%if index < values_hash.keys.size - 1  %},{%end%}{%end%}]
    end

    def set_values(values)
      values_for_super = values[0..-{{values_hash.keys.size + 1}}]
      values_for_this = values.pop {{values_hash.keys.size}}
      super(values_for_super)
      {% for value_name, index in values_hash.keys %}
        value = values_for_this[{{index}}]
        unless value.is_a? ValueNotSet
          if value.is_a? {{values_hash[value_name]}}
              {{value_name.id}}(value as {{values_hash[value_name]}})
          end
        end
      {%end%}
      self.values
    end

    def values=(values)
      set_values(values)
    end

  end

  def values
    [] of Nil
  end

  def set_values(args)
    if args.size > 0
      raise Exception.new "two many arguments provided for value="
    end
    [] of Nil
  end

  def children
    [] of CLTK::ASTNode?|Array(CLTK::ASTNode?)
  end

  def set_children(args)
    if args.size > 0
      raise Exception.new "two many arguments provided for children"
    end
    [] of CLTK::ASTNode?|Array(CLTK::ASTNode?)
  end

  macro children(childrens_hash)
    {% if @type.superclass == CLTK::ASTNode %}
      CHILDREN_NAMES = {{childrens_hash.keys}}
      CHILDREN_DEFAULTS = Array(CLTK::ASTNode?|Array(CLTK::ASTNode?)).new
    {% else %}
      CHILDREN_NAMES = ({{@type.superclass}}::CHILDREN_NAMES as Array(Symbol)) + {{childrens_hash.keys}}
      CHILDREN_DEFAULTS = {{@type.superclass}}::CHILDREN_DEFAULTS.clone
    {% end %}
    {% for child_name, child_type in childrens_hash %}
      def {{child_name.id}}
        @children[CHILDREN_NAMES.rindex(:{{child_name.id}}) as Int32]
      end

      {% if child_type.id.stringify.starts_with? "Array" %}
        CHILDREN_DEFAULTS << [] of CLTK::ASTNode?
        def {{child_name.id}}(value : {{child_type.id}})
          @{{child_name.id}} = value.map do |v|
            v.parent = self if v
            v as CLTK::ASTNode?
          end
          index = CHILDREN_NAMES.rindex(:{{child_name.id}})
          if index
            @children[index] = @{{child_name.id}}
          end
        end
      {% else %}
        CHILDREN_DEFAULTS << nil
        def {{child_name.id}}(value : {{child_type}})
          index = CHILDREN_NAMES.rindex(:{{child_name.id}})
          @{{child_name.id}} = value
          value.parent = self if value
          if index
            @children.not_nil![index] = @{{child_name.id}}
          end
        end
      {% end %}

      def {{child_name.id}}=(value : {{child_type.id}})
        {{child_name.id}}(value)
      end

    {% end %}

    def children
      super() +
      [{% for child_name, index in childrens_hash.keys %}self.{{child_name.id}}{%if index < childrens_hash.keys.size - 1  %},{%end%}{%end%}]
    end

    def set_children(children)
      children_for_super = children[0..-{{childrens_hash.keys.size + 1}}]
      children_for_this = children.pop {{childrens_hash.keys.size}}

      super(children_for_super)
      {% for child_name, index in childrens_hash.keys %}
         child = children_for_this[{{index}}]
         if child
           {% if childrens_hash[child_name].id.stringify.starts_with? "Array" %}
             {{child_name.id}}((child as Array).map do |child_element|
               child_element.parent = self if child_element && child_element.is_a? CLTK::ASTNode
               child_element as ({{childrens_hash[child_name].id.stringify.gsub(/Array\(|\)$/, "").id}})
             end as {{childrens_hash[child_name]}})
           {% else %}
             {{child_name.id}}(child as {{childrens_hash[child_name]}})
             (child as CLTK::ASTNode).parent = self if child
           {% end %}
         end
      {%end%}
      self.children
    end

    def children=(children)
      set_children(children)
    end

    def initialize
      @children = CHILDREN_DEFAULTS.clone
    end

    def self.children_names
      CHILDREN_NAMES.clone
    end

    def self.children_defaults
      CHILDREN_DEFAULTS.clone
    end
  end

  macro inherited
    def initialize(*values)
      @children = CHILDREN_DEFAULTS.clone
      children_values, value_values =
        if ORDER == :custom
          children_values = CHILDREN_NAMES.map do |name|
            index = CUSTOM_ORDER.index(name)
            if index
              values.to_a[index]
            else
              ValueNotSet.new
            end
          end
          value_values = VALUES_NAMES.map do |name|
            index = CUSTOM_ORDER.index(name)
            if index
              values.to_a[index]
            else
              ValueNotSet.new
            end
          end
          {children_values, value_values}
        elsif ORDER == :children
          ## problem here: [1][0..0] == [1][0..-1] :(
          children_values = values.to_a.shift({{@type.id}}::CHILDREN_NAMES.size)
          value_values = values.to_a.pop({{@type.id}}::VALUES_NAMES.size)
          {children_values, value_values}
        elsif ORDER == :values
          children_values = values.to_a.pop({{@type.id}}::CHILDREN_NAMES.size)
          value_values = values.to_a.shift({{@type.id}}::VALUES_NAMES.size)
          {children_values, value_values}
        end as Tuple
      if children_values.size > {{@type}}::CHILDREN_NAMES.size || value_values.size > {{@type}}::VALUES_NAMES.size
        raise Exception.new "#{values} does not match the children, values and order defined for {{@type}}"
      end
      set_children(children_values.not_nil!.map do |val|
                     if val.is_a? CLTK::ASTNode?
                       val
                     elsif val.is_a? Array
                       clean = val.map do |v|
                         begin
                           v as CLTK::ASTNode?
                         rescue
                           raise Exception.new "#{v.inspect} wasn't an Array of CLTK::ASTNode?"
                         end
                       end
                       clean
                     elsif val.is_a? ValueNotSet
                       nil
                     else
                       raise Exception.new "#{val.inspect} wasn't a CLTK::ASTNode"
                     end;
                   end
                  )
      set_values(value_values.not_nil!)
    end
    def initialize
      @children = CHILDREN_DEFAULTS
    end
  end

  def clone
    {% unless @type.stringify =~ /^CLTK::ASTNode/ %}
      thing = {{@type.stringify.gsub(/\+$/, "").id}}.new
      if thing.responds_to? :"children="
        thing.children = children.clone
      end
      if thing.responds_to? :"values="
        thing.values = values.clone
      end
      thing as {{@type}}
    {% else %}
      self
    {% end %}
  end

  # An iterator over the node's children.  The AST may be traversed in
  # the following orders:
  #
  # * Pre-order (:pre)
  # * Post-order (:post)
  # * Level-order (:level)
  #
  # @param [:pre, :post, :level]  order  The order in which to iterate over the tree
  #
  # @return [void]
  def each(order = :pre, &block: CLTK::ASTNode+ -> _ )
    case order
    when :pre
      yield self
      children.flatten.compact.map {|s| s as CLTK::ASTNode }.each do |c|
         c.each(:pre, &block)
       end
    when :post
      children.flatten.compact.map {|s| s as CLTK::ASTNode }.each do |c|
        c.each(:post, &block)
      end
      yield self
    when :level
      level_queue = [self]
      while level_queue.size > 0
        node = level_queue.shift
        yield (node as CLTK::ASTNode)
        level_queue += (node as CLTK::ASTNode).children.flatten.compact
      end
    end
  end
  macro inherited
    def map(&block: CLTK::ASTNode? -> _)
      new_node = {{@type.stringify.gsub(/\+$/, "").id}}.new

      new_node.set_children(children.map do |child|
        case child
        when Array
          child.map(&block) as Array(CLTK::ASTNode?)
        when CLTK::ASTNode
          child.map(&block) as CLTK::ASTNode?
        when Nil
          nil
        end
      end as Array(CLTK::ASTNode?|Array(CLTK::ASTNode?)))

      new_node.set_values(values.clone as Array)
      yield new_node
    end

    def map!(&block: CLTK::ASTNode? -> _)
      set_children(children.map do |child|
        case child
        when Array
          child.map do |c_child|
            if c_child.is_a? CLTK::ASTNode
              c_child.map!(&block) as CLTK::ASTNode?
            else
              nil
            end
          end as Array(CLTK::ASTNode?)
        when CLTK::ASTNode
          child.map!(&block) as CLTK::ASTNode?
        when Nil
          nil
        end
      end as Array(CLTK::ASTNode?|Array(CLTK::ASTNode?)))
      yield self
    end

    def eval
      self
    end

    def explain
      indent = "\n"
      c = children
      v = values
      name = "{{@type}}"
      res = "#{indent}(#{name}"
      if c.size > 0
        res = res + "#{indent}  (children"
        ({{@type.id}}::CHILDREN_NAMES).each do |name|
          child = c[{{@type.id}}::CHILDREN_NAMES.index(name).not_nil!]
          children = if child.is_a? Array
                       child
                     else
                       [child]
                     end
          children.each do |child|
            child = (child as CLTK::ASTNode).to_s.split("\n")
            res = res + "#{indent}    #{name}: #{child[1].to_s}\n"
            child[2..-1].each do |line|
              res = res + "    " + line + "\n"
            end
          end
        end
        res = res + "#{indent}  )"
      end
      if v.size > 0
        res = res + "#{indent}  (values"
        ({{@type.id}}::VALUES_NAMES).each do |name|
          value = (v[{{@type.id}}::VALUES_NAMES.index(name).not_nil!]).to_s
          value = (value.is_a?(String) ? value : "#{value}").split("\n")
          res = res + "#{indent}    #{name}: #{value.first.to_s}" as String
          value[1..-1].each do |line|
            res = res + "    " + line
          end
        end.join as String
        res = res + "#{indent}  )"
      end
      res = res + "#{indent})"
      res as String

    end

  end
  def ==(other : self)
    values == other.values && children == other.children
  end

end
