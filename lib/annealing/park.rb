module Annealing
  class Park
    attr_accessor :atoms, :inspected

    # get centers from polygroups and convert to atoms
    def self.build_from_polygroups(pgs)
      c = Park.new
      pgs.each do |pg|
        pt = pg.center
        next if pt.nil?
        c.atom_at(pt.x, pt.y)
      end
      c
    end

    def initialize
      @atoms = []
      @inspected = nil
    end

    def place_people(people)
      raise("count mismatch") unless people.length == atoms.length
      atoms.zip(people).each do |a,p|
        a.person = p
      end
    end

    def links_with_energy
      links.map {|a,b| [a.person.mismatches(b.person), [a,b]] }
    end

    def energy
      links_with_energy.inject(0){|s,(e,_)| s + e }
    end

    # swap two people randomly
    # so much simpler than in the tutorial!!
    def mutate
      #STDERR.<< "."
      links = walking_neighbors(4)
      swap = links.sample
      a0 = swap[0]
      a1 = swap[1]
      p = a0.person
      a0.person = a1.person
      a1.person = p
      @inspected = [a0,a1]
      self
    end
    def rollback
      #STDERR.<< "!"
      a0, a1 = @inspected
      p = a0.person
      a0.person = a1.person
      a1.person = p
    end

    def atom_at(x,y)
      a = Atom.new
      a.point = Point.new(x,y)
      atoms << a
    end

    def eq(other)
      other.atoms == self.atoms
    end
    alias :== :eq

    def sitting_neighbors
      @sitting_neighbors ||= build_sitting_neighbors
    end
    alias :links :sitting_neighbors

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
      sorted[0...to_take].uniq
    end

  end
end
