require 'spec_helper'
module Annealing
  describe Atom do
    it "has a point" do
      a = Atom.new
      expect(->{ a.point }).to_not raise_exception
    end

    it "is equal with another Atom if its point matches" do
      a1 = Atom.new
      a1.point = Point.new(1,2)

      a2 = Atom.new
      a2.point = Point.new(2,3)

      a3 = Atom.new
      a3.point = Point.new(1,2)

      a4 = Atom.new
      a4.point = Point.new(1,2)

      expect(a1).to_not eq(a2)
      expect(a1).to     eq(a3)
      expect(a1).to     eq(a4)
    end

    describe "#at" do
      it "builds an Atom at a specified point" do
        a = Atom.at(1,2)
        expect(a.point).to   eq(Point.new(1,2))
      end
    end
  end
end
