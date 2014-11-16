module Annealing
  class Placement < Struct.new(:point, :person)
    def self.at(x,y)
      a = Placement.new
      a.point = Point.new(x,y)
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
