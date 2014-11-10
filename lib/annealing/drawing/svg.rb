module Annealing::Drawing
  module SVG
    class << self
      SVG_HEADER = %q{<svg xmlns="http://www.w3.org/2000/svg">}
      SVG_FOOTER = %q{</svg>}
      def polygons_to_svg(*polygroups)
        "#{ SVG_HEADER }#{ polygroups.map{|g| pg(g) }.join }#{ SVG_FOOTER }"
      end

      def pg(polygroup)
        c = polygroup.color || "rgb(20,250,250)"
        polygroup.polys.map{|p| polyline(p, polygroup.fill, c) }.join
      end

      def svg_to_polygons(svg)
        doc = Nokogiri::XML.parse(svg)
        Annealing::Geometry::PolyGroup.new(polys_from_polygons(doc) + polys_from_paths(doc))
      end

      def box_at(p)
        pg = Annealing::Geometry::PolyGroup.new [Annealing::Geometry::Polygon.make(*[[3,3],[3,-3],[-3,-3],[-3,3]].map{|dx,dy| [p.x + dx, p.y + dy] })]
        pg.color = 'black'
        pg
      end

      private

      def polys_from_polygons(doc)
        doc.css('polygon').map do |poly|
          points = poly.attr('points').split(/ /)
          pairs = points.map{|xy| xy.split(/,/).map(&:to_f) }
          Annealing::Geometry::Polygon.make(*pairs)
        end
      end

      def polys_from_paths(doc)
        doc.css('path').map do |poly|
          points = poly.attr('d')
          bits = points.scan(/([\d.]+),([\d.]+)/)
          pairs = bits.map{|(x,y)| [x.to_f, y.to_f] }
          Annealing::Geometry::Polygon.make(*pairs)
        end
      end

      def polyline(p, fill="#cccccc", color="rgb(20,250,250)")
        %Q{<polygon points="#{p.points.map{|pt| point(pt) }.join(" ")}" style="fill:#{fill};stroke:#{color};stroke-width:2"/>}
      end

      def point(p)
        "#{p.x.round},#{p.y.round}"
      end
    end
  end
end
