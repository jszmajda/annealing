$: << "."
$: << "./lib"
require 'annealing'
#require 'pry'

park = Annealing::Drawing::SVG.svg_to_polygons(File.read("haskell/park.svg"))

people = Annealing.load_people("people.txt")

parts = park.allocate(people.length) # @parts is an array of PolyGroups

crystal = Annealing::Simul::Crystal.build_from_polygroups(parts)
crystal.place_people(people)

time_allowed = 1000
#time_allowed = 2

puts "Starting energy: #{crystal.energy}"
puts "Starting temperature: #{crystal.temperature(500,500)}"

time_allowed.times do |i|
  crystal.anneal(i, time_allowed)
end

puts "Final Energy: #{crystal.energy}"
puts "Final Temperature: #{format("%0.3f", crystal.temperature(time_allowed, time_allowed))}"

File.open("rbfinal.svg", 'wb') do |f|
  pgs = crystal.sitting_neighbors.map do |link|
    a, b = link
    pg = Annealing::Geometry::PolyGroup.new([Annealing::Geometry::Polygon.new([a.point, b.point])])
    r,g,b = a.person.similarity_color(b.person).map(&:to_i)
    pg.color = "rgb(#{r},#{g},#{b})"
    pg
  end
  f << Annealing::Drawing::SVG.polygons_to_svg(*pgs.sample(pgs.length))
end
