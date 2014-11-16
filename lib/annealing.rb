require 'nokogiri'

module Annealing
  def self.load_people(data)
    people = eval(data)
    people.map{|answers| Person.new(answers) }
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

  def self.write_svg(filename, park)
    File.open(filename, 'wb') do |f|
      pgs = park.sitting_neighbors.map do |link|
        a, b = link
        pg = PolyGroup.new([Polygon.new([a.point, b.point])])
        r,g,b = a.person.similarity_color(b.person).map(&:to_i)
        pg.color = "rgb(#{r},#{g},#{b})"
        pg
      end
      f << SVG.polygons_to_svg(*pgs.sample(pgs.length))
    end
  end

  module Geometry
  end
  module Drawing
  end
  module Simul
  end
end

require 'annealing/simulated_annealing'
require 'annealing/park_annealer'
require 'annealing/park'
require 'annealing/placement'
require 'annealing/person'
require 'annealing/polygon'
require 'annealing/point'
require 'annealing/poly_group'
require 'annealing/svg'
