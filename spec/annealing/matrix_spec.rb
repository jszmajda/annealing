require 'spec_helper'
module Annealing
  describe Matrix do
    it "is constructed with a list of nodes" do
      nod = Node.new(Point.new(1,2))
      mat = Matrix.new([
        nod
      ])
      expect(mat.nodes.first).to eq nod
    end
    #it "draws to a processing context"
  end
end
