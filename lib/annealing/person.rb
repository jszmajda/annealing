module Annealing
  class Person
    attr_accessor :answers
    def initialize(answers)
      @answers = answers
    end

    def mismatches(op)
      answers.zip(op.answers).select{|a,b| a != b}.length
    end

    def similarity_color(op)
      m = mismatches(op)
      h = answers.length / 2.0
      d = 30 * (h - m).abs
      b = [0, (255-d)].max
      o = [d, 255].min
      if m < h
        [0, o, b]
      else
        [o, 0, b]
      end
    end

    def self.load_people
      dat = File.read(File.join(File.dirname(__FILE__), %w{.. .. people.txt}))
      people = eval(dat)
      people.map{|as| Person.new(as) }
    end
  end
end
