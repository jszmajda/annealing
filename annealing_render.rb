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

def mouse_pressed
  x=mouse_x
  y=mouse_y
  closest = @viz.crystal.atoms.first
  @viz.crystal.atoms.each do |a|
    dx = a.point.x - mouse_x
    dy = a.point.y - mouse_y
    dist = dx.abs + dy.abs
    odx = closest.point.x - mouse_x
    ody = closest.point.y - mouse_y
    odist = odx.abs + ody.abs
    closest = a if dist < odist
  end
  @mover.point = closest
end

class Mover
  attr_accessor :viz, :x, :y, :point
  def initialize(viz)
    @viz = viz
    @x = 0
    @y = 0
    @rad = 15
  end
  def point=(p)
    if p
      @point = p
      @x = p.point.x
      @y = p.point.y
    end
  end

  def draw
    fill(0,150,250)
    ellipse(@x,@y, @rad + 5,@rad + 5)
    fill(0,190,255)
    ellipse(@x,@y, @rad,@rad)

    next_candidates.each do |cand|
      fill(0)
      stroke(200,100,0)
      ellipse(cand.point.x, cand.point.y, 10,10)
    end
  end

  def next_candidates
    return [] unless point
    links = @viz.crystal.neighbor_links
    links.select{|l| l[0] == point || l[1] == point }.flatten.uniq.reject{|a| a == point }
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
