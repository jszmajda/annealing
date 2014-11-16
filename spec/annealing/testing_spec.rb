require 'spec_helper'
describe "doing it all" do
  it "does" do
    root_dir = File.join(File.dirname(__FILE__), '..', '..')
    park = SVG.svg_to_polygons(File.read("#{root_dir}/spec/park.svg"))

    File.open("#{root_dir}/out.svg", 'wb') do |f| 

      parts = park.allocate(158)
      bounds = parts.map do |p|
        l, t, r, b = p.send(:bounding_rect)
        PolyGroup.new([Polygon.make(
          [l,t], [r,t],
          [r,b], [l,b]
        )])
      end
      centers = parts.map(&:center).compact.map{|p| SVG.box_at(p) }

      f << SVG::SVG_HEADER
      f << SVG.pg(park)
      bounds.each{|b| b.color = "rgb(130,130,130)"; b.fill = "rgba(10,10,10,0.1)"; f << SVG.pg(b) }
      centers.map{|c| f << SVG.pg(c) }
      f << SVG::SVG_FOOTER

    end
  end
end
