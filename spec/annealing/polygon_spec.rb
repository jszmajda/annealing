require 'spec_helper'
module Annealing
  describe Polygon do
    it "is a list of points" do
      p = Polygon.new([
        Point.new(1,2),
        Point.new(2,3)
      ])
      expect(p.points.first).to eq Point.new(1,2)
    end

    it "supports equality checking" do
      p1 = Polygon.new([Point.new(1,2), Point.new(2,3)])
      p2 = Polygon.new([Point.new(1,2), Point.new(2,3)])
      p3 = Polygon.new([Point.new(3,2), Point.new(2,3)])
      expect(p1).to eq p2
      expect(p1).to_not eq p3
    end

    it "constructs a little more easily" do
      p = Polygon.make([100,200], [200,300], [100,300])
      p2 = Polygon.new([ Point.new(100,200), Point.new(200,300), Point.new(100,300) ])
      expect(p).to eq(p2)
    end

    it "makes with a Point" do
      p = Polygon.make(Point.new(100,200), [200,300], [100,300])
      p2 = Polygon.new([ Point.new(100,200), Point.new(200,300), Point.new(100,300) ])
      expect(p).to eq(p2)
    end

    describe "#triangulate" do
      let(:square) { Polygon.make([100,200], [200,200], [200,100], [100,100]) }
      it "cuts a square up properly" do
        triangulated = PolyGroup.new([
          Polygon.make([100,200], [200,200], [200,100]),
          Polygon.make([100,200], [200,100], [100,100])
        ])
        expect(square.triangulate).to eq(triangulated)
      end
      it "is idempotent" do
        triangulated = PolyGroup.new([
          Polygon.make([100,200], [200,200], [200,100]),
          Polygon.make([100,200], [200,100], [100,100])
        ])
        expect(square.triangulate.triangulate).to eq(triangulated)
      end
    end
  end
end
