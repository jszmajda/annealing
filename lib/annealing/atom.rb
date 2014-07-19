module Annealing
  class Atom
    attr_accessor :crystal, :point, :person

    def self.at(x,y)
      a = Atom.new
      a.point = Point.new(x,y)
      a
    end

    def eq(other)
      other.point == self.point
    end
    alias :== :eq

    def <=>(other)
      (point.x + point.y) <=> (other.point.x + other.point.y)
    end

    def inspect
      "a.#{point.inspect}"
    end
  end
end
