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
      self.points == other.points
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

  end
end
