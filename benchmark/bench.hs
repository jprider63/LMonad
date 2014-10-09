let timeAction action = getCurrentTime >>= (\t1 -> action >>= (\g -> getCurrentTime >>= (\t2 -> let timeInUnits = (realToFrac $ diffUTCTime t2 t1 :: Float) in return timeInUnits)))

let runData f = randomIO >>= (\randomChar -> if (last (show f)) == randomChar then return () else return ())

-- TODO: deepseq
let timeData d = timeAction (runData d)

seed  <- newStdGen

let randomlist n = newStdGen >>= (return . take n . unfoldr (Just . random))

let n = 30 :: Int
ds1 <- mapM (\_ -> randomlist n >>= return . Set.fromList ) [1..n]
ds2 <- mapM (\_ -> randomlist n >>= return . Set.fromList ) [1..n]
let c1 = Set.fromList ds1
let c2 = Set.fromList ds2
timeData (conjunctionOr c1 c2)
