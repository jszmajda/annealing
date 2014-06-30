module Annealing
  class Atom
    attr_accessor :crystal, :point

    def self.at(x,y)
      a = Atom.new
      a.point = Point.new(x,y)
      a
    end

    def eq(other)
      other.point == self.point
    end
    alias :== :eq

    def inspect
      "a.#{point.inspect}"
    end
  end
end
