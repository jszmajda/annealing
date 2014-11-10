require 'spec_helper'
module Annealing::Simul
  describe Atom do
    it "has a point" do
      a = Atom.new
      expect(->{ a.point }).to_not raise_exception
    end

    it "is equal with another Atom if its point matches" do
      c1 = Crystal.new
      c2 = Crystal.new

      a1 = Atom.new
      a1.point = Annealing::Geometry::Point.new(1,2)

      a2 = Atom.new
      a2.point = Annealing::Geometry::Point.new(2,3)

      a3 = Atom.new
      a3.point = Annealing::Geometry::Point.new(1,2)

      a4 = Atom.new
      a4.point = Annealing::Geometry::Point.new(1,2)

      expect(a1).to_not eq(a2)
      expect(a1).to     eq(a3)
      expect(a1).to     eq(a4)
    end

    describe "#at" do
      it "builds an Atom at a specified point" do
        a = Atom.at(1,2)
        expect(a.point).to   eq(Annealing::Geometry::Point.new(1,2))
      end
    end
  end
end
