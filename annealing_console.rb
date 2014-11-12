$: << "."
$: << "./lib"
require 'annealing'
require 'pry'

park = Annealing::Drawing::SVG.svg_to_polygons(File.read("pk2.svg"))

people = Annealing.load_people("pl2.txt")
#puts "p0: #{people.first.sum}"
#puts "p1: #{people[1].sum}"

#binding.pry
parts = park.allocate(people.length) # @parts is an array of PolyGroups
File.open("rouou.txt", 'wb'){|f|
  parts.each do |pg|
    f << "["
    pg.polys.each do |p|
      f << "\n  ["
      pts = p.points.map do |t|
        "(#{"%0.3f" % t.x},#{"%0.3f" % t.y})"
      end
      f << pts.join(",")
      f << "]"
    end
    f << "\n]\n"
  end
}

File.open("rcnts.txt", 'wb'){|f|
  parts.map(&:center).compact.sort{|a,b| (a.x+a.y) <=> (b.x+b.y)}.each {|c|
    f << "(#{"%0.3f" % c.x},#{"%0.3f" % c.y})\n"
  }
}

crystal = Annealing::Simul::Crystal.build_from_polygroups(parts)
crystal.place_people(people)

#time_allowed = 5000
time_allowed = 2

puts "Starting energy: #{crystal.energy}"
puts "Starting temperature: #{crystal.temperature(500,500)}"

time_allowed.times do |i|
  crystal.anneal(i, time_allowed)
end

puts "Final Energy: #{crystal.energy}"
puts "Final Temperature: #{format("%0.3f", crystal.temperature(time_allowed, time_allowed))}"
