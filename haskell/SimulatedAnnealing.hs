module SimulatedAnnealing (
  EnergyFunction, TemperatureFunction
, TransitionProbabilityFunction, MotionFunction
, anneal, AnnealEnv (AnnealEnv)
) where

import System.Random
import Data.List (foldl')

type EnergyFunction a              = a -> Int
type TemperatureFunction           = Int -> Int -> Float
type TransitionProbabilityFunction = Int -> Int -> Float -> Float
type MotionFunction a              = StdGen -> a -> (StdGen,a)
data AnnealEnv a = AnnealEnv {
  aeEnergy :: EnergyFunction a
, aeTemperature :: TemperatureFunction
, aeTransitionProbability :: TransitionProbabilityFunction
, aeMotion :: MotionFunction a
}


anneal_tick :: MotionFunction a -> TransitionProbabilityFunction -> EnergyFunction a -> Float -> (StdGen,a) -> (StdGen,a)
anneal_tick mf tpf ef t (r,p) = let (r2,p2) = mf r p
                                    (n ,r3) = random r2
                                in (r3,
                                    if n < tpf (ef p) (ef p2) t
                                    then p2
                                    else p)

anneal' :: EnergyFunction a -> MotionFunction a -> TransitionProbabilityFunction -> TemperatureFunction -> Int -> StdGen -> a -> a
anneal' ef mf tpf tf m r s = snd $ foldl' (flip (anneal_tick mf tpf ef)) (r,s) (map (tf m) [0..m])

-- implied extra params time allowed, randGen, starting placement
anneal :: AnnealEnv annealable -> Int -> StdGen -> annealable -> annealable
anneal env = anneal' (aeEnergy env) (aeMotion env) (aeTransitionProbability env) (aeTemperature env)
