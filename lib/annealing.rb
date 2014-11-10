require 'nokogiri'

module Annealing
  def self.load_people
    dat = File.read(File.join(File.dirname(__FILE__), %w{.. people.txt}))
    people = eval(dat)
    people.map{|answers| Annealing::Simul::Person.new(answers) }
  end
  module Geometry
  end
  module Drawing
  end
  module Simul
  end
end

require 'annealing/simul/crystal'
require 'annealing/simul/atom'
require 'annealing/simul/person'
require 'annealing/geometry/polygon'
require 'annealing/geometry/point'
require 'annealing/geometry/poly_group'
require 'annealing/drawing/svg'
