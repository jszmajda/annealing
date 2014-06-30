require 'spec_helper'
module Annealing
  describe Crystal do

    let(:sample) do
      ps = [[53,60], [34,96], [69,102], [104,97], [139,104]]
      c = Crystal.new
      c.atoms = ps.map{|p| a = Atom.new; a.crystal = c; a.point = Point.new(*p); a }
      c
    end

    describe "#atom_at" do
      it "builds an Atom at the specified point" do
        pending
        c = Crystal.new
        c.atom_at(1,2)

        a = Atom.new
        a.point = Point.new(1,2)
        a.crystal = c

        expect(c.atoms).to eq([a])
      end
    end

    describe "#build_from_polygroups" do
      it "constructs a crystal from a list of PolyGroups" do
        pending "todo"
        # get centers from polygroups and convert to atoms
      end
    end

    it "has atoms" do
      c = Crystal.new
      expect(-> { c.atoms }).to_not raise_exception
    end

    describe "#neighbors" do
      it "returns the N nearest neighbors" do
        pending "Crystal"
        multi = complex.allocate(5)
        puts "--"
        multi.map(&:center).each do |c|
          puts "Center: #{c.inspect}"
        end
        expect(1).to eq(0)
      end
    end
  end
end
