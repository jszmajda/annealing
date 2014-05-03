require 'spec_helper'
module Annealing
  describe "doing it all" do
    it "does" do
      root_dir = File.join(File.dirname(__FILE__), '..', '..')
      park = SVG.svg_to_polygons(File.read("#{root_dir}/spec/park.svg"))

      File.open("#{root_dir}/out.svg", 'wb') do |f| 

        parts = park.allocate(200)
        centers = parts.map(&:center).compact.map{|p| SVG.box_at(p) }

        f << SVG::SVG_HEADER
        f << SVG.pg(park)
        centers.map{|c| f << SVG.pg(c) }
        f << SVG::SVG_FOOTER

      end
    end
  end
end
