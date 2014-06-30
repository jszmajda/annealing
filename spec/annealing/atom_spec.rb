require 'spec_helper'
module Annealing
  describe Atom do
    it "has a point" do
      a = Atom.new
      expect(->{ a.point }).to_not raise_exception
    end
    it "has a crystal" do
      a = Atom.new
      expect(->{ a.crystal }).to_not raise_exception
    end

    it "is equal with another Atom if its point matches" do
      c1 = Crystal.new
      c2 = Crystal.new

      a1 = Atom.new
      a1.point = Point.new(1,2)
      a1.crystal = c1

      a2 = Atom.new
      a2.point = Point.new(2,3)
      a2.crystal = c1

      a3 = Atom.new
      a3.point = Point.new(1,2)
      a3.crystal = c1

      a4 = Atom.new
      a4.point = Point.new(1,2)
      a4.crystal = c2

      expect(a1).to_not eq(a2)
      expect(a1).to eq(a3)
      expect(a1).to eq(a4)
    end
  end
end