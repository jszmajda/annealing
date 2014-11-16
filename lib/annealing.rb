require 'nokogiri'

module Annealing
  def self.load_people(data)
    people = eval(data)
    people.map{|answers| Annealing::Simul::Person.new(answers) }
  end

  def self.anneal
    state      = initial_state
    cur_energy = state.energy
    total_time.times do |i|
      t = temperature(total_time, i)

      next_state  = state.mutate
      next_energy = next_state.energy

      if probability(cur_energy, next_energy, t) > rand(0.0..1.0)
        state = next_state
        cur_energy = next_energy
      end
    end
  end

  def self.write_svg(filename, crystal)
    File.open(filename, 'wb') do |f|
      pgs = crystal.sitting_neighbors.map do |link|
        a, b = link
        pg = Annealing::Geometry::PolyGroup.new([Annealing::Geometry::Polygon.new([a.point, b.point])])
        r,g,b = a.person.similarity_color(b.person).map(&:to_i)
        pg.color = "rgb(#{r},#{g},#{b})"
        pg
      end
      f << Annealing::Drawing::SVG.polygons_to_svg(*pgs.sample(pgs.length))
    end
  end

  module Geometry
  end
  module Drawing
  end
  module Simul
  end
end

require 'annealing/simul/simulated_annealing'
require 'annealing/simul/crystal_annealer'
require 'annealing/simul/crystal'
require 'annealing/simul/atom'
require 'annealing/simul/person'
require 'annealing/geometry/polygon'
require 'annealing/geometry/point'
require 'annealing/geometry/poly_group'
require 'annealing/drawing/svg'
