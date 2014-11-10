$: << "."
$: << "./lib"
require 'annealing'

park = Annealing::SVG.svg_to_polygons(File.read("spec/park.svg"))

people = Annealing.load_people
parts = park.allocate(people.length) # @parts is an array of PolyGroups
crystal = Annealing::Crystal.build_from_polygroups(parts)
crystal.randomly_place_people(people)

time_allowed = 5000

puts "Starting energy: #{crystal.energy}"
puts "Starting temperature: #{crystal.temperature(500,500)}"

time_allowed.times do |i|
  crystal.anneal(i, time_allowed)
end

puts "Final Energy: #{crystal.energy}"
puts "Final Temperature: #{format("%0.3f", crystal.temperature(time_allowed, time_allowed))}"
