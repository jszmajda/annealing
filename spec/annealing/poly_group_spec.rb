require 'spec_helper'
module Annealing::Geometry
  describe PolyGroup do
    let(:park) do
      file = File.join(File.dirname(__FILE__), '..', 'park.svg')
      Annealing::Drawing::SVG.svg_to_polygons(File.read(file))
    end
    let(:complex) do
      PolyGroup.new([
        Polygon.make(
          [106.7619, 131.19048],
          [17.190476, 96.809524],
          [22.619047, 71.47619],
          [34.380952, 47.952381],
          [46.142857, 39.809524],
          [157.42857, 105.85714],
          [106.7619, 131.19048]
        )
      ])
    end
    let(:simple) { PolyGroup.new([Polygon.make([10,20],[20,20],[20,5])]) }
    it "constructs from a set of Polygons" do
      polys = [Polygon.make([1,2],[2,3]), Polygon.make([2,3],[3,4])]
      pg = PolyGroup.new([Polygon.make([1,2],[2,3]), Polygon.make([2,3],[3,4])])
      expect(pg.polys).to eq(polys)
    end

    it "supports equality checks" do
      pg1 = PolyGroup.new([Polygon.make([1,2],[2,3]), Polygon.make([2,3],[3,4])])
      pg2 = PolyGroup.new([Polygon.make([1,2],[2,3]), Polygon.make([2,3],[3,4])])
      pg3 = PolyGroup.new([Polygon.make([1,2],[2,4]), Polygon.make([2,3],[3,4])])

      expect(pg1).to eq pg2
      expect(pg1).to_not eq pg3
    end

    it "supports color" do
      pg = PolyGroup.new([Polygon.make([1,2],[2,3]), Polygon.make([2,3],[3,4])])
      pg.color = :blue
      expect(pg.color).to eq :blue
    end

    it "triangulates all polys in the group" do
      chpt = park.triangulate
      expect(chpt.polys.select{|p| p.points.length == 3 }.count).to eq(chpt.polys.count)
    end

    describe "#area" do
      it "returns the area of its polygons" do
        p = PolyGroup.new([
          Polygon.make([0,0], [10,0], [0,10]),
          Polygon.make([10,0], [10,10], [0,10])
        ])

        expect(p.area).to eq(10*10)
      end
    end

    describe "slicing" do
      describe "#slice_x" do
        it "clips properly simple" do
          left = PolyGroup.new([Polygon.make([10,20],[20,20],[20,5])])
          right = PolyGroup.new([])
          expect(simple.slice_x(50)).to eq([left,right])
        end
        it "clips properly complex" do
          left = PolyGroup.new([
            Polygon.make([17.190475,96.809525 ], [50.0,109.403076 ], [22.619047,71.47619]),
            Polygon.make([22.619047,71.47619  ], [50.0,109.403076 ], [50.0,90.90784]),
            Polygon.make([22.619047,71.47619  ], [50.0,90.90784   ], [34.38095,47.95238]),
            Polygon.make([34.38095,47.95238   ], [50.0,90.90784   ], [50.0,65.91429]),
            Polygon.make([34.38095,47.95238   ], [50.0,65.91429   ], [46.142857,39.809525]),
            Polygon.make([46.142857,39.809525 ], [50.0,65.91429   ], [50.0,45.624023]),
            Polygon.make([46.142857,39.809525 ], [50.0,45.624023  ], [50.0,42.098724])
          ])
          right = PolyGroup.new([
            Polygon.make([106.7619,131.19048  ], [50.0,109.403076     ], [50.0,90.90784]),
            Polygon.make([106.7619,131.19048  ], [50.0,90.90784       ], [50.0,65.91428]),
            Polygon.make([106.7619,131.19048  ], [50.0,65.91428       ], [50.0,45.624023]),
            Polygon.make([106.7619,131.19048  ], [50.0,45.624023      ], [157.42857,105.85714]),
            Polygon.make([157.42857,105.85714 ], [50.0,45.624023      ], [50.0,42.09873]),
            Polygon.make([106.7619,131.19048  ], [157.42857,105.85714 ], [106.7619,131.19048])
          ])
          expect(complex.slice_x(50)).to eq([left, right])

        end
      end
      describe "#slice_y" do
        it "clips properly simple" do
          top = PolyGroup.new([Polygon.make([20.0,5.0],[13.333334,15.0],[20.0,15.0])])
          bottom = PolyGroup.new([
            Polygon.make([10.0,20.0 ], [13.333333,15.0 ], [20.0,20.0]),
            Polygon.make([20.0,20.0 ], [13.333333,15.0 ], [20.0,15.0])
          ])
          expect(simple.slice_y(15)).to eq([top,bottom])
        end
        it "clips properly complex" do
          top = PolyGroup.new([
            Polygon.make([34.38095,47.95238   ], [36.161488,50.0 ], [33.35714,50.0]),
            Polygon.make([34.38095,47.95238   ], [36.161488,50.0 ], [46.142857,39.809525]),
            Polygon.make([46.142857,39.809525 ], [36.161488,50.0 ], [52.902874,50.0]),
            Polygon.make([46.142857,39.809525 ], [52.902874,50.0 ], [63.31311,50.0])
          ])
          bottom = PolyGroup.new([
            Polygon.make([106.7619,131.19048  ], [17.190475,96.809525 ], [22.619047,71.47619]),
            Polygon.make([106.7619,131.19048  ], [36.16149,50.0       ], [22.619047,71.47619]),
            Polygon.make([22.619047,71.47619  ], [36.16149,50.0       ], [33.35714,50.0]),
            Polygon.make([106.7619,131.19048  ], [36.16149,50.0       ], [52.902878,50.0]),
            Polygon.make([106.7619,131.19048  ], [52.902878,50.0      ], [157.42857,105.85714]),
            Polygon.make([157.42857,105.85714 ], [52.902878,50.0      ], [63.31311,50.0]),
            Polygon.make([106.7619,131.19048  ], [157.42857,105.85714 ], [106.7619,131.19048])
          ])
          expect(complex.slice_y(50)).to eq([top, bottom])
        end
      end
      describe "#allocate" do
        it "divides the PolyGroup into equal parts" do
          result = PolyGroup.new([Polygon.make([0,0],[202,0],[204,200], [306,302], [110, 302])]).allocate(3)
          expected = [
            PolyGroup.new([ Polygon.make([0.0,0.0], [153.0,0.0], [153.0,150.0]),
                            Polygon.make([0.0,0.0], [153.0,151.0], [153.0,150.0]),
                            Polygon.make([153.0,150.0], [153.0,151.0], [153.0,151.0]),
                            Polygon.make([0.0,0.0], [153.0,151.0], [55.0,151.0]) ]), 
            PolyGroup.new([ Polygon.make([153.0,151.0], [153.0,151.0], [153.0,151.0]),
                            Polygon.make([153.0,151.0], [153.0,151.0], [110.0,302.0]),
                            Polygon.make([110.0,302.0], [153.0,151.0], [55.0,151.0]),
                            Polygon.make([110.0,302.0], [153.0,151.0], [153.0,302.0]) ]), 
            PolyGroup.new([ Polygon.make([202.0,0.0], [153.0,0.0], [204.0,200.0]),
                            Polygon.make([204.0,200.0], [153.0,0.0], [153.0,150.0]),
                            Polygon.make([204.0,200.0], [153.0,150.0], [306.0,302.0]),
                            Polygon.make([306.0,302.0], [153.0,150.0], [153.0,151.0]),
                            Polygon.make([306.0,302.0], [153.0,151.0], [153.0,302.0]) ])
          ]
          # Meh, comparing inspects. Not sure why == isn't working...
          expect(result.inspect).to eq(expected.inspect)
        end
      end
    end

    describe "#center" do
      it "returns the centerpoint of the polygroup" do
        expect(complex.center).to eq(Point.new(87.309523, 85.5))
        expect(simple.center).to eq(Point.new(15, 12.5))
      end
    end

    describe "#bounds" do
      it "returns the boundary points of a polygon" do
        p = PolyGroup.new([
          Polygon.make([0,0], [10,0], [0,10]),
          Polygon.make([10,0], [10,10], [0,10]),
          Polygon.make([5,0], [0,1], [0,10]),
          Polygon.make([50,0], [0,1], [0,10])
        ])

        expected = Polygon.make([50,0],[10,10],[0,10],[0,0])
        expect(p.bounds).to eq(expected)
      end
    end
  end
end
