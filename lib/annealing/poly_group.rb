module Annealing
  class PolyGroup
    attr_accessor :polys, :color
    def initialize(polys)
      @polys = polys
    end

    def ==(other)
      other.polys.sort == self.polys.sort
    end
    def eq(other)
      self.==(other)
    end

    def rainbow!
      self.color = %w{red yellow orange green blue violet}.sample
      self
    end

    def inspect
      "pg[ #{polys.map(&:inspect).join(', ')} ]"
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

    def allocate(n)
      return [PolyGroup.new([])]    if n <= 0
      return [PolyGroup.new(polys)] if n == 1
      t1, t2 = halve_triangles(n)
      a1 = t1.area
      a2 = t2.area
      f = ((n.to_f * a1) / (a1 + a2)).round
      alloc1 = t1.allocate(f)
      alloc2 = t2.allocate(n - f)
      alloc1 + alloc2
    end

    def area
      @area ||= polys.inject(0){|s,p| s + p.area }
    end

    def center
      return nil unless polys.any?
      l,t,r,b = bounding_rect
      mx = (r+l) / 2.0
      my = (b+t) / 2.0
      lh, rh = slice_x(mx)
      th, bh = PolyGroup.new(lh.polys + rh.polys).slice_y(my)
      m = Point.new(mx, my)
      sort = ->(q,s) { q.distance_to(m) <=> s.distance_to(m) }
      all_points = [lh,rh,th,bh].map(&:polys).flatten.map(&:points).flatten
      all_points.sort(&sort).first
    end

    private

    def halve_triangles(n)
      l,t,r,b = bounding_rect
      f = n.to_f
      h = f / 2.0
      if (r - l) > (b - t)
        slice_x((r*h+l*(f-h))/f)
      else
        slice_y((b*h+t*(f-h))/f)
      end
    end

    def bounding_rect
      ps = polys.map(&:points).flatten
      xs = ps.map(&:x)
      ys = ps.map(&:y)
      [xs.min, ys.min, xs.max, ys.max]
    end

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
