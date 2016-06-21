require "../../src/cltk/ast"
require "spec"
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

describe CLTK::ASTNode do

  describe "initialization" do
    it "initializes a simple class" do
      js = JsonString.new(text: "hey a text")
      js.text.should eq("hey a text")
    end

    it "initializes a simple class with array attributes" do
      js1 = JsonString.new(text: "text1")
      js2 = JsonString.new(text: "text2")
      jsarray = JsonArray.new(elements: [js1, js2])
      jsarray.elements.should eq [js1, js2]
    end

    it "initializes a class setting the values hierarchically" do
      js1 = JsonString.new(text: "text1")
      js2 = JsonString.new(text: "text2")
      jsarray = JsonArray.new(elements: [js1, js2])

      js3 = SuperSpecialJsonString.new(text: "this is a text", special: true, child: jsarray)
      js3.text.should eq "this is a text"
      js3.special.should eq true
      js3.child.should eq jsarray
    end
  end

  describe "==" do
    it "can be compared to another value simple case" do
      js1 = JsonString.new(text: "text1")
      js2 = JsonString.new(text: "text1")
      js1.should eq js1
    end

    it "can be compared to another value complex case" do
      js1_a = JsonString.new(text: "text1")
      js2_a = JsonString.new(text: "text2")
      js1_b = JsonString.new(text: "text1")
      js2_b = JsonString.new(text: "text2")

      jsarray_a = JsonArray.new(elements: [js1_a, js2_a])
      jsarray_b = JsonArray.new(elements: [js1_b, js2_b])

      js_a = SuperSpecialJsonString.new(text: "this is a text", special: true, child: jsarray_a)
      js_b = SuperSpecialJsonString.new(text: "this is a text", special: true, child: jsarray_b)

      js_a.should eq js_b

      js1_b = JsonString.new(text: "text2")
      jsarray_b = JsonArray.new(elements: [js1_b, js2_b])
      js_b = SuperSpecialJsonString.new(text: "this is a text", special: true, child: jsarray_b)

      js_a.should_not eq js_b
    end
  end

  describe "values" do
    it "returns a NamedTuple representing the values of the Object" do
      js1 = JsonString.new(text: "text1")
      js1.values.should eq({text: "text1"})
    end

    it "returns a NamedTuple representing the values of the inheriting Object" do
      js1 = JsonString.new(text: "text1")
      js2 = JsonString.new(text: "text2")
      jsarray = JsonArray.new(elements: [js1, js2])
      js_a = SuperSpecialJsonString.new(text: "this is a text", special: true, child: jsarray)
      js_a.values.should eq({
                             text: "this is a text",
                             special: true,
                             child: jsarray
                            })
    end

    it "works with a class in the hierarchy thats not defining its own values" do
      s1 = ChildOfSpecialisedJsonString.new(text: "a text for JsonString", childish: false)
      s1.values.should eq({text: "a text for JsonString", childish: false})
    end
  end

end
