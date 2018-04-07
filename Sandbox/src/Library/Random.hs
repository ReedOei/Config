module Library.Random (randInt, randIndex, rand, rollDice,
                rouletteSelectBy, rouletteSelect,
                choice, shuffle) where
import Library.List (remove)

import System.Random

rand :: (Random a, Floating a) => IO a
rand = getStdRandom $ randomR (0, 1)

rollDice :: (Random a, Integral a) => a -> a -> IO a
rollDice dice sides = do
    res <- mapM (\i -> getStdRandom $ randomR (1, sides)) [1..dice]
    return $ sum res

randInt :: (Random a, Integral a) => a -> a -> IO a
randInt a b = getStdRandom $ randomR (a, b)

randIndex :: [a] -> IO Int
randIndex xs = randInt 0 (length xs - 1)

rouletteSelectBy :: (Ord b, Random b, Floating b) => (a -> b) -> [a] -> IO a
rouletteSelectBy f vs = do
    m <- rand
    let goal = m * total
    case sumUntil vs goal 0 of
        Nothing -> return $ head vs
        Just v -> return v
    where total = sum $ map f vs
          sumUntil [] _ _ = Nothing
          sumUntil (x:xs) goal acc
            | nextAcc >= goal = Just x
            | otherwise = sumUntil xs goal nextAcc
            where nextAcc = f x + acc

rouletteSelect :: (Ord a, Random a, Floating a) => [a] -> IO a
rouletteSelect = rouletteSelectBy id

choice :: [a] -> IO a
choice xs = do
    i <- randIndex xs
    return (xs !! i)

shuffle :: Eq a => [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    x <- choice xs
    nextXs <- shuffle (remove xs x)
    return (x : nextXs)

