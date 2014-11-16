module SimulatedAnnealing (
  EnergyFunction, TemperatureFunction
, ProbabilityFunction, MutationFunction
, anneal, AnnealEnv (AnnealEnv)
) where

import System.Random
import Data.List (foldl')

type TimeAllowed = Int
type CurrentTime = Int
type Temperature = Float
type Energy = Int
type Probability = Float

type EnergyFunction a              = a -> Int
type TemperatureFunction           = TimeAllowed -> CurrentTime -> Temperature
type ProbabilityFunction = Energy -> Energy -> Temperature -> Probability
type MutationFunction a            = StdGen -> a -> (StdGen,a)


data AnnealEnv a = AnnealEnv {
  aeEnergy                  :: EnergyFunction a
, aeTemperature             :: TemperatureFunction
, aeProbability   :: ProbabilityFunction
, aeMutation                :: MutationFunction a
}


tick :: MutationFunction a -> ProbabilityFunction -> EnergyFunction a ->
        (a, StdGen) -> Temperature -> (a, StdGen)
tick mf tpf ef (state, gen) t =
  let (rand, g2)      = randomR (0.0,1.0) gen
      (g3, nextState) = mf g2 state
      shouldSwap      = tpf (ef state) (ef nextState) t > rand
  in (if shouldSwap then nextState else state, g3)

anneal' :: EnergyFunction a -> MutationFunction a -> ProbabilityFunction -> TemperatureFunction -> TimeAllowed -> StdGen -> a -> a
anneal' ef mf tpf tf time gen state = fst $ foldl' tick' (state, gen) temps
  where
    temps = map (tf time) [0..time]
    tick' = tick mf tpf ef

-- implied extra params time allowed, randGen, starting placement
anneal :: AnnealEnv annealable -> TimeAllowed -> StdGen -> annealable -> annealable
anneal env = anneal' (aeEnergy env) (aeMutation env) (aeProbability env) (aeTemperature env)
