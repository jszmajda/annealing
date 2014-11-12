require 'spec_helper'
module Annealing::Simul
  describe Crystal do

    let(:sample) do
      ps = [[53,60], [34,96], [69,102], [104,97], [139,104]]
      c = Crystal.new
      ps.each { |p| c.atom_at(*p) }
      c
    end

    describe "#atom_at" do
      it "builds an Atom at the specified point" do
        c = Crystal.new
        c.atom_at(1,2)

        a = Atom.new
        a.point = Annealing::Geometry::Point.new(1,2)

        expect(c.atoms).to eq([a])
      end
    end

    describe "#build_from_polygroups" do
      it "constructs a crystal from a list of PolyGroups" do
        pgs = Annealing::Geometry::PolyGroup.new([
          Annealing::Geometry::Polygon.make(
            [106.7619, 131.19048],
            [17.190476, 96.809524],
            [22.619047, 71.47619],
            [34.380952, 47.952381],
            [46.142857, 39.809524],
            [157.42857, 105.85714],
            [106.7619, 131.19048]
          )
        ]).allocate(5)
        c1 = Crystal.build_from_polygroups(pgs)

        c2 = Crystal.new
        cents = [
          [47.138192347840516,59.44240117669629],
          [45.238094800000006,98.70815553008887],
          [87.30952300000001,92.51274277912717],
          [126.02837785404404,87.22125424903828],
          [129.38095120000003,116.53407141634611]
        ]
        cents.each {|p| c2.atom_at(*p) }

        expect(c1).to eq(c2)
      end
    end

    it "has atoms" do
      c = Crystal.new
      expect(-> { c.atoms }).to_not raise_exception
    end

    it "is equal to another Crystal with the same atoms" do
      c1 = Crystal.new
      c2 = Crystal.new
      c3 = Crystal.new

      c1.atom_at(1,2)
      c1.atom_at(2,3)

      c2.atom_at(1,2)
      c2.atom_at(2,3)

      c3.atom_at(1,2)
      c3.atom_at(2,4)

      expect(c1).to     eq(c2)
      expect(c1).to_not eq(c3)
    end

    describe "#sitting_neighbors" do
      it "returns a list of links" do
        links = sample.sitting_neighbors

        # links should all have both endpoints
        expect(links.reject {|a,b| a && b }).to eq([])

        expected = [
          [[69.0,  102.0], [104.0, 97.0]],
          [[34.0,  96.0],  [69.0,  102.0]],
          [[104.0, 97.0],  [139.0, 104.0]],
          [[53.0,  60.0],  [34.0,  96.0]],
          [[53.0,  60.0],  [69.0,  102.0]],
          [[53.0,  60.0],  [104.0, 97.0]],
          [[34.0,  96.0],  [104.0, 97.0]],
          [[69.0,  102.0], [139.0, 104.0]],
          [[53.0,  60.0],  [139.0, 104.0]],
          [[34.0,  96.0],  [139.0, 104.0]],
        ].map do |pa, pb|
          [Atom.at(*pa), Atom.at(*pb)]
        end

        expect(links).to eq(expected)
      end
    end
  end
end
