# Author:		Chris Wailes <chris.wailes@gmail.com>
# Project: 	Ruby Language Toolkit
# Date:		2011/04/06
# Description:	This file contains unit tests for the CLTK::Token class.
require "spec"
require "../../src/cltk/token"

describe "Token" do

  describe "#initialize" do
    t0 = CLTK::Token.new(:FOO, "0")
    t1 = CLTK::Token.new(:FOO, "0")
    t2 = CLTK::Token.new(:FOO, "1")
    t3 = CLTK::Token.new(:BAR, "0")
    t4 = CLTK::Token.new(:BAR, "1")

    t0.should eq t1

    t0.should_not eq t2
    t0.should_not eq t3
    t0.should_not eq t4
  end
end
