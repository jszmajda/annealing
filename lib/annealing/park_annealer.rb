module Annealing
  class ParkAnnealer
    attr_accessor :park, :people, :parts

    include SimulatedAnnealing
    def initialize
    end

    def setup(svg, people_data)
      self.park = SVG.svg_to_polygons(svg)
      self.people = Annealing.load_people(people_data)
      self.parts = park.allocate(people.length) # @parts is an array of PolyGroups
      park = Park.build_from_polygroups(parts)
      park.place_people(people)
      self.state = park
    end

    def rollback
      state.rollback
    end

    # falls over time, is a nice gradual slope
    def temperature(max, current)
      50 * Math.exp(0 - (5 * ( current / max.to_f)))
    end

    # values that lower the energy are always > 1.0
    # values that raise it will fall between 0..1
    # Why not `(e1 - e2) > 0` ?
    # we're avoiding being stuck at a local minimum that is worse than
    # the global one.
    #
    # When T tends to zero, the probability P(e, e', T) must tend to zero
    # if e' > e and to a positive value otherwise. For sufficiently small
    # values of T, the system will then increasingly favor moves that go
    # "downhill" (i.e., to lower energy values), and avoid those that go
    # "uphill." With T=0 the procedure reduces to the greedy algorithm,
    # which makes only the downhill transitions.
    #
    def probability(e1, e2, temperature)
      Math.exp( (e1 - e2) / temperature ) > rand(0.0..1.0)
    end

  end
end
