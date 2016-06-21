struct NamedTuple
  def -(other : U)
    {% begin %}
    NamedTuple.new(
      {% for key in (T.keys + U.keys).uniq %}
          {% if !U.keys.includes?(key) %}
            {{key}}: self[:{{key}}],
          {% end %}
      {% end %}
    )
    {% end %}
  end
  def -(other : U)
    {% begin %}
    NamedTuple.new(
      {% for key in (T.keys + U.keys).uniq %}
          {% if !U.keys.includes?(key) %}
            {{key}}: self[:{{key}}],
          {% end %}
      {% end %}
    )
    {% end %}
  end

end

def named(text : String)
  pp text
end

def test(**options)
  if (options - {text: String, bool: Bool}).size != 0
    puts "hey"
  end
#  named(**(options - {text: String, bool: Bool}))
end


def test2(**options)
  pp options
end

test(text: "hey", bool: true)
