require 'nokogiri'
require 'annealing/crystal'
require 'annealing/atom'
require 'annealing/person'
require 'annealing/polygon'
require 'annealing/point'
require 'annealing/svg'
require 'annealing/poly_group'
module Annealing
  def self.load_people
    dat = File.read(File.join(File.dirname(__FILE__), %w{.. people.txt}))
    people = eval(dat)
    people.map{|answers| Person.new(answers) }
  end
end
