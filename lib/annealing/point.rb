module Annealing
  class Point
    attr_accessor :x, :y
    def initialize(x,y)
      @x, @y = x.to_f, y.to_f
    end

    ROUNDING = 5
    def ==(other)
      (other.x / self.x).round(ROUNDING) === 1.0 &&
      (other.y / self.y).round(ROUNDING) === 1.0
    end
    def eql(other)
      self.==(other)
    end

    def inspect
      "(#{x},#{y})"
    end

    def <=>(other)
      (other.x + other.y) <=> (self.x + self.y)
    end
  end
end
