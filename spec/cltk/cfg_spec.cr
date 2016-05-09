# Author:		Chris Wailes <chris.wailes@gmail.com>
# Project: 	Ruby Language Toolkit
# Date:		2011/04/06
# Description:	This file contains unit tests for the CLTK::CFG class.

############
# Requires #
############


#######################
# Classes and Modules #
#######################
require "spec"
require "../../src/cltk/cfg"

describe "CLTK::CFG" do
  grammar = CLTK::CFG.new

  grammar.production(:s) do
    clause("A G D")
    clause("A a C")
    clause("B a D")
    clause("B G C")
  end

  grammar.production(:a, "b")
  grammar.production(:b, "G")

  it "should test callback" do
    grammar1 = CLTK::CFG.new
    call_count = 0

    grammar1.callback do |type, which, p|
      type.should eq :optional

      case call_count
      when 0 then which.should eq :empty
      when 1 then which.should eq :nonempty
      end

      call_count += 1
      nil
    end


    grammar1.production(:a, "A?") { |a| a }
    call_count.should eq 2

    call_count = 0
    grammar1.callback do |type, which, p|
#      refute_nil(p)

      case call_count
      when 0
	type.should eq :elp
	which.should eq :empty

      when 1
	type.should eq :elp
	which.should eq :nonempty

      when 2
	type.should eq :nelp
	which.should eq :single

      when 3
	type.should eq :nelp
	which.should eq :multiple
      end

      call_count += 1
      nil
    end

    grammar1.production(:a, "A*") { |a| a }
    call_count.should eq 4

    call_count = 0
    grammar1.callback do |type, which, p|
#      refute_nil(p)
      type.should eq :nelp

      case call_count
      when 0 then which.should eq :single
      when 1 then which.should eq :multiple
      end

      call_count += 1
      nil
    end

    grammar1.production(:a, "A+") { |a| a }
    call_count.should eq 2
  end

  it "test_first_set" do
    grammar.first_set("s").each do |sym|
      ["A", "B"].should contain(sym)
    end

    grammar.first_set("b").should eq ["G"]
    grammar.first_set("a").should eq ["G"]
  end

  it "test_follow_set" do
    grammar.follow_set("s").should eq ["EOS"]

    grammar.follow_set("a").each do |sym|
      ["C", "D"].should contain(sym)
    end

    grammar.follow_set("b").each do |sym|
      ["C", "D"].should contain(sym)
    end
  end

  it "test_is_nonterminal" do
    CLTK::CFG.is_nonterminal?("lowercase").should eq true
    CLTK::CFG.is_nonterminal?("UPERCASE").should eq false
  end

  it "test_is_terminal" do
    CLTK::CFG.is_terminal?(:lowercase).should eq false
    CLTK::CFG.is_terminal?(:UPERCASE).should eq true
  end

  it "test_item" do
    i0 = CLTK::CFG::Item.new(0, 0, "a", ["b", "C", "D", "e"])
    i1 = i0.copy

    i1.should eq i0
    i0.at_end?.should be_false
    i0.next_symbol.should eq "b"

    i0.advance

    i0.should_not eq i1
    i0.at_end?.should be_false
    i0.next_symbol.should eq "C"

    i0.advance
    i0.at_end?.should be_false
    i0.next_symbol.should eq "D"

    i0.advance
    i0.at_end?.should be_false
    i0.next_symbol.should eq "e"

    i0.advance
    i0.at_end?.should be_true
    i0.next_symbol.should be_nil
  end

  it "test_production" do
    p0 = CLTK::CFG::Production.new(0, "a", ["b", "C", "D", "e"])
    p1 = p0.copy

    p0.should eq p1
    p0.last_terminal.should eq "D"
  end

  it "sdfasd" do
    #grammar = CLTK::CFG.new
    #
    #grammar.production(:s) do
    #  clause("A G D")
    #  clause("A a C")
    #  clause("B a D")
    #  clause("B G C")
    #end

    #grammar.production("a", "b")
    #grammar.production("b", "G")
    #puts grammar.first_set("A")
    #puts grammar.follow_set "a"
    #puts grammar.nonterms
    #puts grammar.productions
    #puts grammar.symbols
    #puts grammar.terms
  end
end
