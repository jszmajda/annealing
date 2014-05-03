require 'spec_helper'
module Annealing
  describe "doing it all" do
    it "does" do
      root_dir = File.join(File.dirname(__FILE__), '..', '..')
      park = SVG.svg_to_polygons(File.read("#{root_dir}/spec/park.svg"))

      File.open("#{root_dir}/out.svg", 'wb') do |f| 

        parts = park.allocate(200)
        f << Annealing::SVG.polygons_to_svg(*parts.each(&:rainbow!))

      end
    end
  end
end
