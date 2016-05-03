require "spec"
require "../../src/cltk/ast"


class AString < CLTK::ASTNode
  values(
    {text: String|Nil}
  )
end
class AStringWithChild < AString
  ORDER = :values
  children({
             child: AString
           })
end
class AThing < CLTK::ASTNode
  children({
             string: AString|Nil,
             string_not_nil: AString
           })
end

class ASecondThing < AThing
  children({
             strings: Array(AString?)
           })
end

class AThirdThing < ASecondThing
  children({
             stringies: Array(AString?)
           })
end

class ASimpleCVThing < CLTK::ASTNode
 children({
             string: AString|Nil
           })
 values({
           text: String
         })
end

class AValueThing < ASecondThing
  children({
             stringies: Array(AString?)
           })
  values({
           text: String,
           namess: Array(String)
         })
end

class ASecondValueThing < AValueThing
  values({
           more_names: Array(Symbol)
         })
end

class AThirdValueThing < AValueThing
  ORDER = :values
  values({
           thing: AValueThing
         })
end

class ACustomValueThing < AValueThing
  ORDER = :custom
  custom_order(:string, :stringies, :text)
end

describe "CLTK::AST" do

  describe "children" do
    it "should have the right children_names in the right order" do
      AThing.children_names.should eq [:string, :string_not_nil]
      ASecondThing.children_names.should eq [:string, :string_not_nil, :strings]
      AThirdThing.children_names.should eq [:string, :string_not_nil, :strings, :stringies]
    end

    it "should initialize defaults" do
      AThing.children_defaults.should eq [nil, nil]
      ASecondThing.children_defaults.should eq [nil, nil, [] of CLTK::ASTNode?]
      AThirdThing.children_defaults.should eq [nil, nil, [] of CLTK::ASTNode?, [] of CLTK::ASTNode?]
    end

    it "should have the right values_names in the right order" do
      AValueThing::VALUES_NAMES.should eq [:text, :namess]
      ASecondValueThing::VALUES_NAMES.should eq [:text, :namess, :more_names]
    end
  end

  describe "children initialization, getters & setters" do

    string = AString.new
    string2 = AString.new
    athing = ASecondThing.new
    string3 = AString.new
    strings = [AString.new, AString.new]

    it "should initialize children with default values" do
      athing.children.should eq ASecondThing.children_defaults
    end

    it "should have a working getter and setter for AString|Nil" do
      (athing.string = string).should eq string
      athing.string.should eq string
      (athing.string = nil).should eq nil
      athing.string.should eq nil
    end

    it "should have a working getter and setter for Array(ChildType)" do
      athing.strings = strings
      athing.strings.should eq strings
    end

    it "should allow to set all the children" do
      athing.children = [string, string, [] of CLTK::ASTNode?]
      athing.children.size.should eq 3
      athing.string.should eq string
      athing.string_not_nil.should eq string
      athing.strings.should eq [] of CLTK::ASTNode?
#      athing.children.should eq [string, string, [] of CLTK::ASTNode?]
    end

    it "should set the parent association of the assigned children" do
      athing.children = [string, string2, [string3]]
      string.parent.should eq athing
      string2.parent.should eq athing
      string3.parent.should eq athing
    end

    it "should find the root of a tree from any of its children" do
      bthing = athing.clone
      bthing.children = [string.clone, string2.clone, [string3.clone]]
      ((bthing.strings as Array).first as CLTK::ASTNode).root.should eq bthing
      athing.children = [string, string2, [string]]
      string.root.should eq athing
      ((bthing.strings as Array).first as CLTK::ASTNode).root.should eq athing
    end

  end

  describe "value initialization, getters & setters" do
    vthing = AValueThing.new
    svthing = ASecondValueThing.new

    it "should initialize values with default values" do
      vthing.values.should eq [nil, [] of String]
    end

    it "should have a working getter and setter" do
      vthing.text.should eq nil
      vthing.text = "a text"
      vthing.text.should eq "a text"
    end

    it "should have a working getter and setter for Array(ValueType)" do
      vthing.namess.should eq [] of String
      (vthing.namess = ["hey", "jo"]).should eq ["hey", "jo"]
    end

    it "should allow to get all the values" do
      vthing.values = ["my text", ["some", "names"]]
      vthing.values.should eq ["my text", ["some", "names"]]
    end

    it "should allow to set all the values" do
      (vthing.values = ["my text", ["some", "names"]]).should eq ["my text", ["some", "names"]]
      vthing.values.should eq ["my text", ["some", "names"]]
    end

    it "should allow to set/get all the values of different types" do
      (svthing.values = ["hey text", ["my rex", "some rubbish"], [:ant, :just, :nothing]]).should eq ["hey text", ["my rex", "some rubbish"], [:ant, :just, :nothing]]
      svthing.values.should eq ["hey text", ["my rex", "some rubbish"], [:ant, :just, :nothing]]
    end
  end

  describe "initialize methods should work as defined in the classes order" do
    string = AString.new
    string2 = AString.new
    vthing = AValueThing.new

    context ":children" do
      it "should assign the right ORDER CONST to the class" do
        ASecondValueThing::ORDER.should eq :children
      end

      pending "should work with :children order" do
        asv = ASecondValueThing.new string, string, [] of CLTK::ASTNode?, [string], "hey text", ["my rex", "some rubbish"], [:ant, :just, :nothing]
        #
        # the print is necessary, otherwise the value isnt available
        asv.children.map { |x| pp x; x }.should eq [string, string, [] of CLTK::ASTNode?, [string]]
        asv.values.should eq ["hey text", ["my rex", "some rubbish"], [:ant, :just, :nothing]]
      end
    end

    context ":values" do

      it "should assign the right ORDER CONST to the class" do
        AThirdValueThing::ORDER.should eq :values
      end

      it "should work with :values order" do
        ttvthing = AThirdValueThing.new "hey text", ["my rex", "some rubbish"], vthing, string, string2, [] of CLTK::ASTNode?, [string]
        ttvthing.children.should eq [string, string2, [] of CLTK::ASTNode?, [string]]
        ttvthing.values.should eq ["hey text", ["my rex", "some rubbish"], vthing]
      end
    end

    context ":custom" do

      it "should assign the right ORDER CONST to the class" do
        ACustomValueThing::ORDER.should eq :custom
      end
      it "should assign the right CUSTOM_ORDER CONST to the class" do
        ACustomValueThing::CUSTOM_ORDER.should eq [:string, :stringies, :text]
      end

      it "should work with :custom order" do
        acvt = ACustomValueThing.new
        acvt.string_not_nil.should eq(nil)
        acvt = ACustomValueThing.new(string, [string, string], "text")
        acvt.text.should eq "text"
        acvt.stringies.should eq [string, string]
        acvt.string.should eq string
        acvt.children.should eq [string, nil, [] of CLTK::ASTNode?, [string,string]]
        acvt.strings.should eq([] of CLTK::ASTNode?)
        acvt.string_not_nil.should eq(nil)
      end
    end
  end

  describe "#==" do
    string = AString.new "hey"
    string2 = AString.new "hey"
    athing = ASimpleCVThing.new string
    bthing = ASimpleCVThing.new string2

    it "works" do
      ASimpleCVThing::CHILDREN_NAMES.should eq [:string]
      ASimpleCVThing::VALUES_NAMES.should eq [:text]
    end

    it "returns true, if the children and values are the same" do
      athing.string.should eq string
      athing.should eq bthing
    end

    it "returns false, if the children differ" do
      athing.string = AString.new "my text"
      athing.should_not eq bthing
      athing.string = AString.new "hey"
      athing.should eq bthing
      athing.string = nil
      athing.should_not eq bthing
    end

    it "returns false, if the values differ" do
      s1 = AString.new "text"
      s2 = AString.new "text2"
      s1.should_not eq s2
      s2.text = "text"
      s1.should eq s2
    end
  end

  describe "#each" do
    string = AString.new "string"
    string_not_nil = AString.new "string_not_nil"
    strings_first = AString.new("strings_1")
    strings_second = AString.new("strings_2")
    strings = [strings_first, strings_second]
    athing = ASecondThing.new string, string_not_nil, strings

    it "works with :pre" do
      order = [] of CLTK::ASTNode
      athing.each(:pre) do |child|
        order << child
      end
      order.should eq [athing, string, string_not_nil, strings_first, strings_second]
    end

    it "works with :post" do
      order = [] of CLTK::ASTNode
      athing.each(:post) do |child|
        order << child
      end
      order.should eq [string, string_not_nil, strings_first, strings_second, athing]
    end

    it "works with :level" do
      order = [] of CLTK::ASTNode
      string_with_child = AStringWithChild.new "string_wit_child", string
      athing = ASecondThing.new string_with_child, string_not_nil, strings
      athing.each(:level) do |child|
        order << child
      end
      # each level:    < 1. >  < ------------------------- 2. ------------------------------- >  < 3. >
      order.should eq [athing, string_with_child, string_not_nil, strings_first, strings_second, string]
    end
  end

  describe "#map" do
    string = AString.new "string"
    string_not_nil = AString.new "string_not_nil"
    strings_first = AString.new("strings_1")
    strings_second = AString.new("strings_2")
    strings = [strings_first, strings_second]
    athing = ASecondThing.new string, string_not_nil, strings

    it "can be used to clone" do
      bthing = athing.map do |child|
        child
      end
      bthing.not_nil!.values.should eq athing.values
      bthing.not_nil!.children.first.should eq athing.children.first
    end
  end

  describe "#map!" do
    string = AString.new "string"
    string_not_nil = AString.new "string_not_nil"
    strings_first = AString.new("strings_1")
    strings_second = AString.new("strings_2")
    strings = [strings_first, strings_second]
    athing = ASecondThing.new string, string_not_nil, strings

    it "can be used to clone" do
      bthing = athing.clone
      athing.map! do |child|
        if child.is_a? AString
          new = AStringWithChild.new "the new text", child
        end
        new
      end
      athing.should_not eq bthing
      athing.children.map{ |c| c.is_a?(AStringWithChild) ? c.text : "empty"}.should eq ["the new text", "the new text", "empty"]
    end
  end
end
