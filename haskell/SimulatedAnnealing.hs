module SimulatedAnnealing (
  EnergyFunction, TemperatureFunction
, TransitionProbabilityFunction, MutationFunction
, anneal, AnnealEnv (AnnealEnv)
) where

import System.Random
import Data.List (foldl')

type EnergyFunction a              = a -> Int
type TemperatureFunction           = Int -> Int -> Float
type TransitionProbabilityFunction = Int -> Int -> Float -> Float
type MutationFunction a              = StdGen -> a -> (StdGen,a)

type TimeAllowed = Int

data AnnealEnv a = AnnealEnv {
  aeEnergy                  :: EnergyFunction a
, aeTemperature             :: TemperatureFunction
, aeTransitionProbability   :: TransitionProbabilityFunction
, aeMutation                :: MutationFunction a
}

type Temperature = Float

tick :: MutationFunction a -> TransitionProbabilityFunction -> EnergyFunction a ->
        (a, StdGen) -> Temperature -> (a, StdGen)
tick mf tpf ef (state, gen) t =
  let (rand, g2)      = randomR (0.0,1.0) gen
      (g3, nextState) = mf g2 state
      shouldSwap      = tpf (ef state) (ef nextState) t > rand
  in (if shouldSwap then nextState else state, g3)

anneal' :: EnergyFunction a -> MutationFunction a -> TransitionProbabilityFunction -> TemperatureFunction -> Int -> StdGen -> a -> a
anneal' ef mf tpf tf time gen state = fst $ foldl' tick' (state, gen) temps
  where
    temps = map (tf time) [0..time]
    tick' = tick mf tpf ef

-- implied extra params time allowed, randGen, starting placement
anneal :: AnnealEnv annealable -> TimeAllowed -> StdGen -> annealable -> annealable
anneal env = anneal' (aeEnergy env) (aeMutation env) (aeTransitionProbability env) (aeTemperature env)
