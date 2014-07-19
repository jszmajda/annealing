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

    def randomly_place_people(people)
      raise("count mismatch") unless people.length == atoms.length
      atoms.zip(people).each do |a,p|
        a.person = p
      end
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

    # let shortestLinks :: Int -> [Link] -> [Link]
    # shortestLinks n = (take n).(sortBy $ comparing linkLength)
    #  where linkLength [a,b] = distance a b
    def walking_neighbors(min_points)
      my_neighbors = ->(a) do
        all_links = (atoms - [a]).map{|atom| [a, atom].sort }
        sorted = sort_links(all_links)
        sorted[0...min_points]
      end
      atoms.flat_map(&my_neighbors).uniq
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
    def build_neighbor_links
      all_links = atoms.combination(2)
      to_take = (2 * atoms.length)
      sorted = sort_links(all_links)
      sorted[0...to_take].uniq
    end

  end
end
