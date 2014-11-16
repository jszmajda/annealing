$: << "."
$: << "./lib"
require 'annealing'
#require 'pry'

ca = Annealing::ParkAnnealer.new
ca.setup(
  File.read("haskell/park.svg"),
  File.read("people.txt"))

time_allowed = 10_000
#time_allowed = 2

puts "Starting energy: #{ca.energy}"
puts "Starting temperature: #{ca.temperature(time_allowed,0)}"

ca.anneal(time_allowed)

puts "Final Energy: #{ca.energy}"
puts "Final Temperature: #{format("%0.3f", ca.temperature(time_allowed, time_allowed))}"

Annealing.write_svg('rbfinal.svg', ca.state)
