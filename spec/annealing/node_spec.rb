require 'spec_helper'
module Annealing
  describe Node do
    #it "is constructed with 
    it "has a point" do
      n = Node.new(Point.new(1,2))
      expect(n.point).to eq Point.new(1,2)
    end
    #it "has edges"
    #it "has an atom"
  end
end
