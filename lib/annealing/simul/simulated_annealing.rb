module Annealing::Simul
  module SimulatedAnnealing
    attr_accessor :state

    def anneal(total_time)
      total_time.times do |cur_time|
        anneal_tick(total_time, cur_time)
      end
    end

    def anneal_tick(total_time, cur_time)
      cur_energy = state.energy
      t = temperature(total_time, cur_time)

      next_state  = state.mutate
      next_energy = next_state.energy

      if probability(cur_energy, next_energy, t)
        self.state = next_state
        cur_energy = next_energy
      else
        rollback if respond_to? :rollback
      end
    end

    def energy
      state.energy
    end
  end
end
