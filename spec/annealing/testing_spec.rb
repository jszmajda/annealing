require 'spec_helper'
describe "doing it all" do
  it "does" do
    root_dir = File.join(File.dirname(__FILE__), '..', '..')
    park = Annealing::Drawing::SVG.svg_to_polygons(File.read("#{root_dir}/spec/park.svg"))

    File.open("#{root_dir}/out.svg", 'wb') do |f| 

      parts = park.allocate(158)
      bounds = parts.map do |p|
        l, t, r, b = p.send(:bounding_rect)
        Annealing::Geometry::PolyGroup.new([Annealing::Geometry::Polygon.make(
          [l,t], [r,t],
          [r,b], [l,b]
        )])
      end
      centers = parts.map(&:center).compact.map{|p| Annealing::Drawing::SVG.box_at(p) }

      f << Annealing::Drawing::SVG::SVG_HEADER
      f << Annealing::Drawing::SVG.pg(park)
      bounds.each{|b| b.color = "rgb(130,130,130)"; b.fill = "rgba(10,10,10,0.1)"; f << Annealing::Drawing::SVG.pg(b) }
      centers.map{|c| f << Annealing::Drawing::SVG.pg(c) }
      f << Annealing::Drawing::SVG::SVG_FOOTER

    end
  end
end
