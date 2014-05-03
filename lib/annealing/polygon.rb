module Annealing
  class Polygon
    attr_accessor :points
    def initialize(points)
      @points = points
    end

    def self.make(*pairs)
      new(pairs.map{|p| p.respond_to?(:x) ? p : Point.new(*p) })
    end

    def ==(other)
      self.points.sort == other.points.sort
    end

    def eq(other)
      self.==(other)
    end

    # Doesn't handle concave polygons
    def triangulate(set = points)
      return PolyGroup.new([]) if set.length < 3
      a,b,c = set.take(3)
      rest = set[3..-1]
      PolyGroup.new([Polygon.new([a,b,c])] + triangulate([a,c] + rest).polys)
    end

    def area
      @area ||= (points.zip(points.rotate).inject(0) { |sum,(a,b)|
        sum + (a.x * b.y - b.x * a.y)
      } * 0.5).abs
    end

    def inspect
      "<<#{points.map(&:inspect).join('..')}>>"
    end

    # this is a crappy, simple sort
    def <=>(other)
      other.send(:pointsum) <=> self.send(:pointsum)
    end

    private

    def pointsum
      points.inject(0){ |s, p| s + p.x + p.y }
    end

  end
end
