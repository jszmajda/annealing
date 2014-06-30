module Annealing
  class Atom
    attr_accessor :crystal, :point

    def eq(other)
      other.point == self.point
    end
    alias :== :eq
  end
end
