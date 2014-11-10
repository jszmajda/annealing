module Annealing::Simul
  class Atom < Struct.new(:crystal, :point, :person)
    def self.at(x,y)
      a = Atom.new
      a.point = Annealing::Geometry::Point.new(x,y)
      a
    end

    def eq(other)
      other.point == self.point
    end
    alias :== :eq

    def <=>(other)
      point <=> other.point
    end

    def inspect
      "a.#{point.inspect}"
    end
  end
end
