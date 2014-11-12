module Annealing::Simul
  class Crystal
    attr_accessor :atoms

    # get centers from polygroups and convert to atoms
    def self.build_from_polygroups(pgs)
      c = Crystal.new
      pgs.each do |pg|
        pt = pg.center
        next if pt.nil?
        c.atom_at(pt.x, pt.y)
      end
      c
    end

    def initialize
      @atoms = []
    end

    def place_people(people)
      raise("count mismatch") unless people.length == atoms.length
      atoms.zip(people).each do |a,p|
        a.person = p
      end
    end

    def energy
      sitting_neighbors.map {|a,b| a.person.mismatches(b.person) }.inject(0){|s,e| s + e }
    end

    # swap two people randomly
    # so much simpler than in the tutorial!!
    def motion
      links = walking_neighbors(4)
      swap = links.sample
      a0 = swap[0]
      a1 = swap[1]
      p = a0.person
      a0.person = a1.person
      a1.person = p
      [a0,a1]
    end
    def rollback(changed)
      a0, a1 = changed
      p = a0.person
      a0.person = a1.person
      a1.person = p
    end

    # TemperatureFunction = Int -> Int -> Float
    def temperature(current, max)
      50 * Math.exp(0 - (5 * ( current / max.to_f)))
    end

    # TransitionProbabilityFunction = Int -> Int -> Float -> Float
    def transition_probability(e1, e2, temperature)
      Math.exp( (e1 - e2) / temperature )
    end

    def anneal(cur_time, max_time)
      anneal_tick(temperature(cur_time, max_time))
    end

    def anneal_tick(t)
      current_state_energy = energy
      inspected = motion
      next_state_energy = energy

      tp = transition_probability( current_state_energy, next_state_energy, t)

      n = rand(0.0..1.0)
      if n < tp
        # keep next state
        inspected
      else
        rollback(inspected)
        []
      end
    end

    def atom_at(x,y)
      a = Atom.new
      a.point = Annealing::Geometry::Point.new(x,y)
      atoms << a
    end

    def eq(other)
      other.atoms == self.atoms
    end
    alias :== :eq

    def sitting_neighbors
      @sitting_neighbors ||= build_sitting_neighbors
    end

    def walking_neighbors(min_points)
      @walking_neighbors ||= begin
      my_neighbors = ->(a) do
        all_links = (atoms - [a]).map{|atom| [a, atom].sort }
        sorted = sort_links(all_links)
        sorted[0...min_points]
      end
      atoms.flat_map(&my_neighbors).uniq
                             end
    end

    private

    def sort_links(links)
      links.sort_by do |link|
        link[0].point.distance_to(link[1].point)
      end
    end

    # using `combination` leads to performance superior to haskell
    # version, but I have to change the formula from 4 * points to
    # 2 * points to get a comparable output.
    def build_sitting_neighbors
      all_links = atoms.combination(2)
      to_take = (2 * atoms.length)
      sorted = sort_links(all_links)
      r = sorted[0...to_take].uniq
      #puts r.inspect
      r
    end

  end
end
