require 'spec_helper'
module Annealing
  describe Placement do
    it "has a point" do
      a = Placement.new
      expect(->{ a.point }).to_not raise_exception
    end

    it "is equal with another Placement if its point matches" do
      a1 = Placement.new
      a1.point = Point.new(1,2)

      a2 = Placement.new
      a2.point = Point.new(2,3)

      a3 = Placement.new
      a3.point = Point.new(1,2)

      a4 = Placement.new
      a4.point = Point.new(1,2)

      expect(a1).to_not eq(a2)
      expect(a1).to     eq(a3)
      expect(a1).to     eq(a4)
    end

    describe "#at" do
      it "builds an Placement at a specified point" do
        a = Placement.at(1,2)
        expect(a.point).to   eq(Point.new(1,2))
      end
    end
  end
end
