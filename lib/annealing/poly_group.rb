module Annealing
  class PolyGroup
    attr_accessor :polys, :color
    def initialize(polys)
      @polys = polys
    end

    def ==(other)
      other.polys == self.polys
    end
    def eq(other)
      self.==(other)
    end

    def triangulate
      threed = polys.map do |p|
        td = p.triangulate
        td.respond_to?(:polys) ? td.polys : td
      end
      g = PolyGroup.new(threed.flatten)
      g.color = self.color
      g
    end

    def slice_x(x)
      partition = ->(p) { x > p.x }
      centerpoint = ->(p1, p2) do
        Point.new( x, p1.y + (p2.y - p1.y) * (x - p1.x) / (p2.x - p1.x) )
      end
      slice(partition, centerpoint, self.triangulate)
    end

    def slice_y(y)
      partition = ->(p) { y > p.y }
      centerpoint = ->(p1, p2) do
        Point.new( p1.x + (p2.x - p1.x) * (y - p1.y) / (p2.y - p1.y), y)
      end
      slice(partition, centerpoint, self.triangulate)
    end

    def rainbow!
      self.color = %w{red yellow orange green blue violet}.sample
    end

    private

    def slice(partition, centerpoint, triangles)
      antipartition = ->(p){ !partition[p] }
      [ clipping(partition,     centerpoint, triangles),
        clipping(antipartition, centerpoint, triangles) ]
    end

    def clipping(partition, centerpoint, triangles)
      PolyGroup.new(triangles.polys.map do |t|
        inside, outside = t.points.partition( &partition )
        clip_triangles(centerpoint, inside, outside)
      end.flatten)
    end

    def clip_triangles(centerpoint, inside, outside)
      case inside.length
      when 0
        []
      when 1
        a, b, c = inside[0], outside[0], outside[1]
        [Polygon.make( a, centerpoint[a,b], centerpoint[a,c] )]
      when 2
        a, b, c = inside[0], inside[1], outside[0]
        [Polygon.make( a, centerpoint[a,c], b), Polygon.make(b, centerpoint[a,c], centerpoint[b,c])]
      when 3
        [Polygon.make(*inside)]
      end
    end
  end
end
