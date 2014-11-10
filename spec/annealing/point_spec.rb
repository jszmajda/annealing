require 'spec_helper'
module Annealing::Geometry
  describe Point do
    it "is an x-y coordinate" do
      p = Point.new(1,2)
      expect(p.x).to eq 1
      expect(p.y).to eq 2
    end

    describe "#==" do
      it "supports equality checking" do
        expect(Point.new(1,2)).to eq Point.new(1,2)
        expect(Point.new(1,2)).to_not eq Point.new(2,2)
      end

      it "allows sufficiently close points to be equal" do
        expect(Point.new(1.1001001, 5)).to eq(Point.new(1.100104, 5))
        expect(Point.new(5, 1.100101)).to eq(Point.new(5, 1.100104))
      end

      it "allows comparing floats to fixnums" do
        expect(Point.new(1.0, 5)).to eq(Point.new(1, 5))
        expect(Point.new(5, 1.0)).to eq(Point.new(5, 1))
      end

      it "allows comparison with 0" do
        expect(Point.new(1.0, 0)).to eq(Point.new(1, 0))
        expect(Point.new(0,1.0)).to eq(Point.new(0,1))
      end
    end

    it "can be sorted" do
      unsort = [Point.new(1,2), Point.new(4,4), Point.new(2,3)]
      sorted = [Point.new(1,2), Point.new(2,3), Point.new(4,4)]
      expect(unsort.sort.reverse).to eq(sorted)
    end

    describe "#distance_to" do
      it "returns the distance to another point" do
        p1 = Point.new(0,0)
        p2 = Point.new(3,0)
        expect(p1.distance_to(p2)).to eq 3
      end
    end

  end
end
