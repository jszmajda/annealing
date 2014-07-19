$: << "."
$: << "./lib"
require 'annealing'
WID=640
HEI=480
def setup
  size WID,HEI
  @viz = Viz.new
  @mover = Mover.new(@viz)
end

def draw
  background(60,50,55)

  @viz.draw
  @mover.draw
end

class Mover
  attr_accessor :viz
  def initialize(viz)
    @viz = viz
    @x = 0
    @y = 0
    @rad = 15
    choose_first_point
    @next = @point
    choose_next_point
  end

  def draw
    update

    fill(0,150,250)
    ellipse(@x,@y, @rad + 5,@rad + 5)
    fill(0,190,255)
    ellipse(@x,@y, @rad,@rad)

    next_candidates.each do |cand|
      fill(0)
      stroke(200,100,0)
      ellipse(cand.point.x, cand.point.y, 10,10)
    end
    fill(0)
    stroke(200,200,0)
    ellipse(@next.point.x, @next.point.y, 13,13)
  end

  def update
    choose_next_point if at_next_point?
    move_to_next_point
  end

  def move_to_next_point
    @x += @dx
    @y += @dy
  end

  attr_reader :point

  def choose_next_point
    @point = @next

    #puts "starting choose_next_point"
    #puts "candidates: #{candidates.inspect}"
    @next = next_candidates.sample
    #puts "@point is #{@point.inspect} Next is #{@next.inspect}"
    distance = Math.sqrt(((@next.point.x - @x) ** 2) + ((@next.point.y - @y) ** 2))
    @dx = (@next.point.x - @x) / (distance / 2.0)
    @dy = (@next.point.y - @y) / (distance / 2.0)
  end

  def next_candidates
    links = @viz.crystal.neighbor_links
    links.select{|l| l[0] == point || l[1] == point }.flatten.uniq.reject{|a| a == point }
  end

  def at_next_point?
    @next.nil? || ( (@x - @next.point.x).abs < 2 && (@y - @next.point.y).abs < 2 )
  end

  def choose_first_point
    a = @viz.crystal.atoms.sample
    @x = a.point.x
    @y = a.point.y
    @point = a
  end
end

class Viz

  def initialize
    @park = Annealing::SVG.svg_to_polygons(File.read("spec/park.svg"))
    @ptri = @park.triangulate
    @tick = 0
    @parts = @park.allocate(num_alloc) # @parts is an array of PolyGroups
  end

  def draw
    ptri
    #mesh
    draw_crystal
    @tick += 1
  end

  def crystal
    @crystal ||= Annealing::Crystal.build_from_polygroups(@parts)
  end

  def draw_crystal
    centers      crystal.atoms
    center_links crystal.neighbor_links
  end

  def num_alloc
    #[@tick, 150].min
    150
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
    stroke(254,0,0)
    links.each do |a,b|
      line(a.point.x, a.point.y, b.point.x, b.point.y)
    end
  end

  def mesh
    stroke_weight(1)
    stroke(230)
    no_fill
    @parts.each do |pg|
      begin_shape
      pg.bounds.points.each do |p|
        vertex(p.x, p.y)
      end
      end_shape
      #pg.polys.each do |p|
      #  triangle(
      #    p.points[0].x, p.points[0].y,
      #    p.points[1].x, p.points[1].y,
      #    p.points[2].x, p.points[2].y
      #  )
      #end
    end
  end

  def ptri
    fill(070,120,070)
    stroke_weight(2)
    stroke(120,100,100)
    @ptri.polys.each do |p|
      triangle(
        p.points[0].x, p.points[0].y,
        p.points[1].x, p.points[1].y,
        p.points[2].x, p.points[2].y
      )
    end
  end
end
