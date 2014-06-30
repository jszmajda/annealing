module Annealing
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

    def atom_at(x,y)
      a = Atom.new
      a.point = Point.new(x,y)
      a.crystal = self
      atoms << a
    end

    def eq(other)
      other.atoms == self.atoms
    end
    alias :== :eq

    def neighbor_links
      @neighbor_links ||= build_neighbor_links
    end

    private

    # using `combination` leads to performance superior to haskell
    # version, but I have to change the formula from 4 * points to
    # 2 * points to get a comparable output.
    def build_neighbor_links
      all_links = atoms.combination(2)
      to_take = (2 * atoms.length)
      sorted = all_links.sort_by do |link|
        link[0].point.distance_to(link[1].point)
      end
      sorted[0..to_take].uniq
    end

  end
end
