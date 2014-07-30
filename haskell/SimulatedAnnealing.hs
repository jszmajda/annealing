module SimulatedAnnealing (EnergyFunction, TemperatureFunction, TransitionProbabilityFunction, MotionFunction) where
import System.Random

type EnergyFunction a              = a -> Int
type TemperatureFunction           = Int -> Int -> Float
type TransitionProbabilityFunction = Int -> Int -> Float -> Float
type MotionFunction a              = StdGen -> a -> (StdGen,a)
