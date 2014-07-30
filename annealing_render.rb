$: << "."
$: << "./lib"
require 'annealing'

def setup
  size 640, 480
  @viz = Viz.new
end

def draw
  background(230)
  @viz.draw
end

class Viz
  def initialize
    @park = Annealing::SVG.svg_to_polygons(File.read("spec/park.svg"))
    @park_triangles = @park.triangulate
    @tick = 0
    @done = false
    people = Annealing::Person.load_people
    @parts = @park.allocate(people.length) # @parts is an array of PolyGroups
    crystal.randomly_place_people(people)
    @start_energy = crystal.energy
    @time_allowed = 100_000
    puts "Starting energy: #{crystal.energy}"
    @step = 50
    @sleep = 0
    puts "Starting temperature: #{crystal.temperature(500,500)}"
  end

  def draw
    draw_polygon_triangles(@park_triangles)
    #draw_mesh
    draw_crystal

    inspected = []
    #50.times do
    unless @done
      @step.times do
        inspected += crystal.anneal(@tick, @time_allowed)
        @tick += 1
      end

      inspected.uniq.each do |atom|
        stroke(180)
        fill(0,0,0,0)
        ellipse(atom.point.x, atom.point.y, 20, 20)
      end
    end
    @done = true if @tick > @time_allowed
    textSize(32)
    fill(0)
    text("Current Energy: #{crystal.energy}", 10, 400)
    text("Current Temperature: #{format("%0.3f", crystal.temperature(@tick, @time_allowed))}", 10, 430)
    text("Start Energy: #{@start_energy}", 10, 460)

    draw_progress_bar(@tick, @time_allowed)
    sleep @sleep
  end

  def draw_progress_bar(from, to)
    stroke_weight(1)
    stroke(150)
    fill(255)
    rect(10,340,600,15)

    pct = (from / to.to_f)
    tx = 600 * pct
    fill(150)
    rect(10,340,tx,15)
  end

  def crystal
    @crystal ||= Annealing::Crystal.build_from_polygroups(@parts)
  end

  def draw_crystal
    centers      crystal.atoms
    center_links crystal.neighbor_links
  end

  def centers(atoms)
    stroke_weight(3)
    stroke(80,180,200)
    fill(150)
    atoms.each do |c|
      p = c.point
      ellipse(p.x,p.y, 5,5)
    end
  end

  def center_links(links)
    stroke_weight(2)
    #stroke(254,0,0)
    links.each do |a,b|
      color = a.person.similarity_color(b.person)
      stroke(*color)
      line(a.point.x, a.point.y, b.point.x, b.point.y)
    end
  end

  def draw_mesh
    stroke_weight(1)
    stroke(230)
    no_fill
    @parts.each do |pg|
      begin_shape
      pg.bounds.points.each do |p|
        vertex(p.x, p.y)
      end
      end_shape
    end
  end

  def draw_polygon_triangles(ptri)
    fill(255)
    stroke_weight(2)
    stroke(255)
    ptri.polys.each do |p|
      triangle(
        p.points[0].x, p.points[0].y,
        p.points[1].x, p.points[1].y,
        p.points[2].x, p.points[2].y
      )
    end
  end
end
