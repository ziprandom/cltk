require "../spec_helper"

class JsonExpression < CLTK::ASTNode; end
class JsonNull < JsonExpression; end

class JsonString < JsonExpression
  values({text: String})
end

class SpecialisedJsonstring < JsonString; end

class ChildOfSpecialisedJsonString < SpecialisedJsonstring
  values({childish: Bool})
end

class SpecialJsonString < JsonString
  values({special: Bool})
end

class SuperSpecialJsonString < SpecialJsonString
  values({child: JsonExpression})
end

class JsonArray < JsonExpression
  values({elements: Array(JsonExpression)})
end

class Parent < CLTK::ASTNode
  values({name: String,
          child_a: Child,
          child_b: Child})
  traverse(:children, :child_a)
  traverse(:children, :child_b)
end

class Child < CLTK::ASTNode
  values({name: String,
          siblings: Array(Child)})
  traverse(:children, :siblings)
end


describe CLTK::ASTNode do

  describe "traversal" do
    c1 = Child.new("child_1", [] of Child)
    # here we create a circle of reference
    # to make sure no infinite loops are created
    c2 = Child.new("child_2", [c1] of Child)
    c1.siblings = [c2]
    p = Parent.new("parent", c1, c2)

    it "works" do
      visited = [] of CLTK::ASTNode

      p.map_children do |c|
        if c.responds_to? :name
          c.name = "visited " + c.name
        end
        visited << c
        c
      end

      visited.should eq [c2, c1, p]
      c1.name.should eq "visited child_1"
      c2.name.should eq "visited child_2"
      p.name.should eq "visited parent"
    end
  end
  describe "initialization" do
    it "initializes a simple class" do
      js = JsonString.new(text: "hey a text")
      js.text.should eq("hey a text")
    end

    it "initializes a simple class with array attributes" do
      js1 = JsonString.new(text: "text1").as(JsonExpression)
      js2 = JsonString.new(text: "text2").as(JsonExpression)
      jsarray = JsonArray.new(elements: [js1, js2])
      jsarray.elements.should eq [js1, js2]
    end

    it "initializes a class setting the values hierarchically" do
      js1 = JsonString.new(text: "text1").as(JsonExpression)
      js2 = JsonString.new(text: "text2").as(JsonExpression)
      jsarray = JsonArray.new(elements: [js1, js2])

      js3 = SuperSpecialJsonString.new(text: "this is a text", special: true, child: jsarray)
      js3.text.should eq "this is a text"
      js3.special.should eq true
      js3.child.should eq jsarray
    end
  end

  describe "==" do
    it "can be compared to another value simple case" do
      js1 = JsonString.new(text: "text1").as(JsonExpression)
      js2 = JsonString.new(text: "text1").as(JsonExpression)
      js1.should eq js1
    end

    it "can be compared to another value complex case" do
      js1_a = JsonString.new(text: "text1").as(JsonExpression)
      js2_a = JsonString.new(text: "text2").as(JsonExpression)
      js1_b = JsonString.new(text: "text1").as(JsonExpression)
      js2_b = JsonString.new(text: "text2").as(JsonExpression)

      js1_a.should eq js1_b
      js2_a.should eq js2_b

      jsarray_a = JsonArray.new(elements: [js1_a, js2_a])
      jsarray_b = JsonArray.new(elements: [js1_b, js2_b])

      jsarray_a.should eq jsarray_b

      js_a = SuperSpecialJsonString.new(text: "this is a text", special: true, child: jsarray_a)
      js_b = SuperSpecialJsonString.new(text: "this is a text", special: true, child: jsarray_b)

      js_a.should eq js_b

      js1_b = JsonString.new(text: "text2")
      jsarray_b = JsonArray.new(elements: [js1_b, js2_b])
      js_b = SuperSpecialJsonString.new(text: "this is a text", special: true, child: jsarray_b)

      js_a.should_not eq js_b
    end
  end

  describe "clone" do
    js1 = JsonString.new(text: "text1").as(JsonExpression)
    js2 = JsonString.new(text: "text2").as(JsonExpression)
    jsarray = JsonArray.new(elements: [js1, js2])
    js = SuperSpecialJsonString.new(text: "this is a text", special: true, child: jsarray)
    js_b = js.clone
    js_b.should eq js
    js_b.child.should eq js.child
    js_b.child.as(JsonArray).elements << JsonString.new(text: "text3")
    js_b.should_not eq js
  end

  pending "to_json" do
    json = <<-JSON
    {
      "text": "this is a text",
      "special": true,
      "child": {
        "elements": [
          {
            "text": "text1"
          },
          {
            "text": "text2"
          }
        ]
      }
    }
    JSON
    js1 = JsonString.new(text: "text1").as(JsonExpression)
    js2 = JsonString.new(text: "text2").as(JsonExpression)
    jsarray = JsonArray.new(elements: [js1, js2])
    js = SuperSpecialJsonString.new(text: "this is a text", special: true, child: jsarray)
    js.to_pretty_json.should eq json
  end

  describe "values" do
    it "returns a NamedTuple representing the values of the Object" do
      js1 = JsonString.new(text: "text1").as(JsonExpression)
      js1.values.should eq({text: "text1"})
    end

    it "returns a NamedTuple representing the values of the inheriting Object" do
      js1 = JsonString.new(text: "text1").as(JsonExpression)
      js2 = JsonString.new(text: "text2").as(JsonExpression)
      jsarray = JsonArray.new(elements: [js1, js2])
      js_a = SuperSpecialJsonString.new(text: "this is a text", special: true, child: jsarray)
      js_a.values.should eq({
                             text: "this is a text",
                             special: true,
                             child: jsarray
                            })
    end

    it "has a nice, readable string representation" do
      js1 = JsonString.new(text: "text1").as(JsonExpression)
      js2 = JsonString.new(text: "text2").as(JsonExpression)
      jsarray = JsonArray.new(elements: [js1, js2])
      js_a = SuperSpecialJsonString.new(text: "this is a text", special: true, child: jsarray)
      js_a.inspect.should eq(
                         %{SuperSpecialJsonString(text: "this is a text", special: true, child: JsonArray(elements: [JsonString(text: "text1"), JsonString(text: "text2")]))}
                       )
    end

    it "works with a class in the hierarchy thats not defining its own values" do
      s1 = ChildOfSpecialisedJsonString.new(text: "a text for JsonString", childish: false)
      s1.values.should eq({text: "a text for JsonString", childish: false})
    end
  end

end
