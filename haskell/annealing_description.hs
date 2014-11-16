temperature :: Int -> Int -> Float
probability :: Int -> Int -> Float -> Float
mutate      :: StdGen -> a -> (StdGen, a)
energy      :: a -> Int

tick :: (a, StdGen) -> Float -> (a, StdGen)
tick (state, gen) t =
  let (rand,      g2) = randomR (0.0,1.0) gen
      (nextState, g3) = mutate state g2
      e1              = energy state
      e2              = energy nextState
      shouldSwap      = probability e1 e2 t > rand
  in (if shouldSwap then nextState else state, g3)

anneal :: a -> Int -> StdGen -> a
anneal initState time gen =
  fst $ foldl' tick (initState, gen) (map (temperature time) [0..time])
