$: << "."
$: << "./lib"
require 'pry'
require 'annealing'
WID=640
HEI=650
def setup
  size WID,HEI
  @viz = Viz.new
end

def draw
  background(230)
  @viz.draw
  sleep 0.001
end

def key_pressed
  if key == "r"
    @viz = Viz.new
  end
end

class Viz

  def initialize
    @annealer = Annealing::Simul::CrystalAnnealer.new
    @annealer.setup(
      File.read("haskell/park.svg"),
      File.read("people.txt"))
    @park_triangles = @annealer.park.triangulate
    @tick = 0
    @done = false
    @start_energy = @annealer.energy
    @time_allowed = 5000
    @energy_graph = EnergyGraph.new(10, 480, 300, 160, @annealer)
    @temperature_graph = TemperatureGraph.new(320,480,300,160, @annealer, @time_allowed)
    puts "Starting energy: #{@annealer.energy}"
    puts "Starting temperature: #{@annealer.temperature(0,@time_allowed)}"
  end

  def draw
    park_triangles
    draw_crystal

    @energy_graph.draw
    @temperature_graph.draw(@tick)

    unless @done
      @annealer.anneal_tick(@time_allowed, @tick)
      @tick += 1
      #end
      @annealer.state.inspected.each do |atom|
        stroke(0)
        fill(0,0,0,0)
        ellipse(atom.point.x, atom.point.y, 20, 20)
      end
    end
    @done = true if @tick > @time_allowed

    progress_bar(@tick, @time_allowed)
    textSize(32)
    fill(0)
    text("Current Energy: #{@annealer.energy}", 10, 400)
    text("Current Temperature: #{format("%0.3f", @annealer.temperature(@time_allowed, @tick))}", 10, 430)
    text("Start Energy: #{@start_energy}", 10, 460)
  end

  def progress_bar(current, max)
    bar_width = 600
    pct = current / max.to_f
    progress_width = width * pct

    stroke_weight 1
    stroke 150
    fill 255
    rect 10, 340, bar_width, 15


    fill 150
    rect 10, 340, progress_width, 15
  end

  def draw_crystal
    centers      @annealer.state.atoms
    center_links @annealer.state.sitting_neighbors
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

  def mesh
    stroke_weight(1)
    stroke(230)
    no_fill
    @annealer.parts.each do |pg|
      begin_shape
      pg.bounds.points.each do |p|
        vertex(p.x, p.y)
      end
      end_shape
    end
  end

  def park_triangles
    fill(255)
    stroke_weight(2)
    stroke(255)
    @park_triangles.polys.each do |p|
      triangle(
        p.points[0].x, p.points[0].y,
        p.points[1].x, p.points[1].y,
        p.points[2].x, p.points[2].y
      )
    end
  end
end

class EnergyGraph
  attr_accessor :x, :y, :wid, :hei, :annealer
  attr_accessor :x_scale, :y_scale, :pad
  def initialize(x,y, wid, hei, annealer)
    @x, @y, @wid, @hei = x, y, wid, hei
    @annealer = annealer
    @pad = 10
    #@ticksize = 2
    @ymax = annealer.state.links_with_energy.map{|(e,_)| e}.max
    @x_scale = (wid - (2*pad)) / annealer.state.links.count.to_f
    @y_scale = (hei - (2*pad)) / @ymax
  end
  def draw
    stroke_weight 1
    no_fill
    stroke(160)
    box
    axes
    columns
  end

  def columns
    links = annealer.state.links_with_energy
    links.sort_by{|(e,_)| e }.reverse.each_with_index do |(e,link),i|
      column(i, e, link)
    end
  end

  def column(i, energy, link)
    stroke_weight 1
    color = link[0].person.similarity_color(link[1].person)
    stroke(*color)
    fill(*color)

    cx = (i * @x_scale) + (x + pad)
    cwid = @x_scale
    cy = y + (2 * pad) + ((@ymax - energy) * @y_scale)
    chei = energy * @y_scale

    rect(cx,cy,cwid,chei)
  end

  def axes
    x_axis
    y_axis
  end

  def x_axis
    ly = y + hei - pad
    minx = x + pad
    maxx = x + wid - pad
    line(minx, ly, maxx, ly)
    #10.times do |i|
    #  dx = (i * ((maxx - minx) / 9)) + minx
    #  line(dx, ly - @ticksize, dx, ly + @ticksize)
    #end
  end

  def y_axis
    lx = x + pad
    miny = y + pad
    maxy = y + hei - pad
    line(lx, miny, lx, maxy)
    #10.times do |i|
    #  dy = (i * ((maxy - miny) / 9)) + miny
    #  line(lx - @ticksize, dy, lx + @ticksize, dy)
    #end
  end

  def box
    rect(x,y,wid,hei)
  end
end

class TemperatureGraph
  attr_accessor :x, :y, :wid, :hei,
    :annealer, :pad, :ymax, :y_scale, :x_scale,
    :time_allowed
  def initialize(x,y, wid, hei, annealer, time_allowed)
    @x, @y, @wid, @hei = x, y, wid, hei
    @pad = 10
    @annealer = annealer
    @time_allowed = time_allowed
    @ymax = @annealer.temperature(time_allowed, 0)
    @x_scale = (wid - (2*pad)) / time_allowed.to_f
    @y_scale = (hei - (2*pad)) / ymax.to_f
    puts "x_scale: #{x_scale} y_scale: #{y_scale} ymax: #{ymax}"
    calc_line_points
  end

  def calc_line_points
    @temp_line = (wid - 2*pad).times.map do |i|
      curdot_coords(i / x_scale)
    end
  end

  def draw(cur)
    stroke_weight 1
    no_fill
    stroke(160)
    axes
    box

    stroke(60,60,60)
    fill(60,60,60)
    temp_line

    stroke(250,0,0)
    fill(250,0,0)
    dx,dy = curdot_coords(cur)
    ellipse(dx,dy, 5,5)
  end

  def temp_line
    @temp_line.each do |(dx,dy)|
      rect(dx,dy,1,1)
    end
  end

  def curdot_coords(cur_tick)
    cur_temp = annealer.temperature(time_allowed, cur_tick)
    dy = y + pad + ymax * y_scale - cur_temp * y_scale
    dx = (cur_tick * x_scale) + x + pad
    [dx,dy]
  end

  def box
    rect(x,y,wid,hei)
  end
  def axes
    x_axis
    y_axis
  end

  def x_axis
    ly = y + hei - pad
    minx = x + pad
    maxx = x + wid - pad
    line(minx, ly, maxx, ly)
    #10.times do |i|
    #  dx = (i * ((maxx - minx) / 9)) + minx
    #  line(dx, ly - @ticksize, dx, ly + @ticksize)
    #end
  end

  def y_axis
    lx = x + pad
    miny = y + pad
    maxy = y + hei - pad
    line(lx, miny, lx, maxy)
    #10.times do |i|
    #  dy = (i * ((maxy - miny) / 9)) + miny
    #  line(lx - @ticksize, dy, lx + @ticksize, dy)
    #end
  end
end
