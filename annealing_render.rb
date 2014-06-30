$: << "."
$: << "./lib"
require 'annealing'
WID=640
HEI=480
def setup
  size WID,HEI
  @viz = Viz.new
end

def draw
  background(60,50,55)

  @viz.draw
end

class Viz

  def initialize
    @park = Annealing::SVG.svg_to_polygons(File.read("spec/park.svg"))
    @ptri = @park.triangulate
    @tick = 0

  end

  def draw
    if(@lna != num_alloc)
      @parts = @park.allocate(num_alloc) # @parts is an array of PolyGroups
      @lna = num_alloc
    end
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
