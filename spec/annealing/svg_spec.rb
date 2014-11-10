require 'spec_helper'
module Annealing::Drawing
  describe SVG do
    let(:sample_svg) {
      %q{<svg xmlns="http://www.w3.org/2000/svg"><polygon points="100,100 200,100 200,200 100,200" style="fill:#cccccc;stroke:rgb(20,250,250);stroke-width:2"/><polygon points="200,200 300,200 300,300 200,300" style="fill:#cccccc;stroke:rgb(20,250,250);stroke-width:2"/></svg>}.strip
    }
    let(:complex_svg) {
      <<-EOT
<svg xmlns:svg="http://www.w3.org/2000/svg" xmlns="http://www.w3.org/2000/svg" version="1.0"
   width="600" height="342"
   id="svg2160">
  <defs id="defs2163" />
  <path d="M 106.7619,131.19048 L 17.190476,96.809524 L 22.619047,71.47619 L 34.380952,47.952381 L 46.142857,39.809524 L 157.42857,105.85714 L 106.7619,131.19048 z " style="opacity:1;color:#000000;fill:#80ff80;fill-opacity:1;fill-rule:nonzero;stroke:#008000;stroke-width:2.5;stroke-linecap:round;stroke-linejoin:round;marker:none;marker-start:none;marker-mid:none;marker-end:none;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1;visibility:visible;display:inline;overflow:visible;enable-background:accumulate" id="path2199" />
</svg>
      EOT
    }
    let(:sample_polys) {
      Annealing::Geometry::PolyGroup.new([
        Annealing::Geometry::Polygon.new([
          Annealing::Geometry::Point.new(100,100),
          Annealing::Geometry::Point.new(200,100),
          Annealing::Geometry::Point.new(200,200),
          Annealing::Geometry::Point.new(100,200),
        ]),
        Annealing::Geometry::Polygon.new([
          Annealing::Geometry::Point.new(200,200),
          Annealing::Geometry::Point.new(300,200),
          Annealing::Geometry::Point.new(300.0,300),
          Annealing::Geometry::Point.new(200,300),
        ]),
      ])
    }
    it "writes a list of Polygons into a simple SVG" do
      output = SVG.polygons_to_svg(sample_polys)
      expect(output).to eq sample_svg
    end
    it "can make a box poly" do
      pg = Annealing::Geometry::PolyGroup.new([
        Annealing::Geometry::Polygon.make(
          [3 , 3] , [3 , -3],
          [-3, -3], [-3, 3]
        )
      ])
      pg.color = 'black'
      expect(SVG.box_at(Annealing::Geometry::Point.new(0,0))).to eq(pg)
    end
    it "reads a simple SVG into a list of Polygons" do
      polys = SVG.svg_to_polygons(sample_svg)
      expect(polys).to eq sample_polys
    end

    it "reads a slightly more complex SVG into a list of Polygons" do
      ps = Annealing::Geometry::PolyGroup.new([
        Annealing::Geometry::Polygon.make(
          [106.7619, 131.19048],
          [17.190476, 96.809524],
          [22.619047, 71.47619],
          [34.380952, 47.952381],
          [46.142857, 39.809524],
          [157.42857, 105.85714],
          [106.7619, 131.19048]
        )
      ])
      polys = SVG.svg_to_polygons(complex_svg)
      expect(polys).to eq ps
    end
  end
end
