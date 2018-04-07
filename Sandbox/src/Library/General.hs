module Library.General where

import Control.Monad.Trans

import qualified Data.Foldable as F
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.IORef
import qualified Data.Map as Map
import Data.Ratio
import qualified Data.Set as Set

import Library.List (setAt)

import System.IO
import System.IO.Unsafe
import System.Random

roughMatch :: (Floating a, Ord a) => a -> a -> a -> Bool
roughMatch tolerance a b = (a + tolerance) >= b && (a - tolerance) <= b

percentMatch :: (Floating a, Ord a) => a -> a -> a -> Bool
percentMatch tolerance a b = (a - b) / a < tolerance

findAllMaybe :: (a -> Maybe Bool) -> [a] -> [a]
findAllMaybe _ [] = []
findAllMaybe f (x:xs) = case f x of
                        Just True -> x : findAllMaybe f xs
                        Just False -> findAllMaybe f xs
                        Nothing -> findAllMaybe f xs

findMaybe :: (a -> Maybe Bool) -> [a] -> Maybe a
findMaybe f [] = Nothing
findMaybe f (x:xs) = case f x of
                    Just True -> Just x
                    Just False -> findMaybe f xs
                    Nothing -> findMaybe f xs

triples :: [a] -> [(a, a, a)]
triples [] = []
triples [_] = []
triples [a,b] = []
triples (a:b:c:xs) = (a, b, c) : triples xs

anyMaybe :: (a -> Maybe Bool) -> [a] -> Bool
anyMaybe _ [] = False
anyMaybe f (x:xs) = case f x of
                        Just True -> True
                        Just False -> anyMaybe f xs
                        Nothing -> anyMaybe f xs

findIndexVal :: (a -> Bool) -> [a] -> Maybe (Int, a)
findIndexVal f xs = find (f . snd) $ zip [0..] xs

wrap :: (Ord a, Num a) => (a, a) -> a -> a
wrap (minVal, maxVal) v
    | v >= maxVal = v - maxVal
    | v < minVal = maxVal + v
    | otherwise = v

clamp :: (Ord a, Num a) => (a, a) -> a -> a
clamp (minVal, maxVal) v
    | v > maxVal = maxVal
    | v < minVal = minVal
    | otherwise = v

scanlWhile :: (b -> Bool) -> (b -> a -> b) -> b -> [a] -> [b]
scanlWhile test f v xs = takeWhile test $ scanl f v xs

collectWhile :: (a -> Bool) -> (a -> a -> a) -> a -> [a] -> [(a, a)]
collectWhile test f = collectWhile'
    where collectWhile' i [x] = [(i, x)]
          collectWhile' i (x:xs)
            | test res = (i, x) : collectWhile' res xs
            | otherwise = []
            where res = f i x

collectUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
collectUntil test f v
    | test v = [v]
    | otherwise = v : collectUntil test f (f v)

isInRange :: Ord a => a -> (a, a) -> Bool
isInRange v (a, b) = a <= v && v < b

combinePair :: (a -> b) -> (b -> b -> c) -> (a, a) -> c
combinePair f c (a, b) = f a `c` f b

bothInPair :: (a -> Bool) -> (a, a) -> Bool
bothInPair f = combinePair f (&&)

eitherInPair :: (a -> Bool) -> (a, a) -> Bool
eitherInPair f = combinePair f (||)

filterMapMaybe :: (a -> Maybe b) -> [a] -> [b]
filterMapMaybe _ [] = []
filterMapMaybe f (x:xs) = case f x of
                        Nothing -> filterMapMaybe f xs
                        Just v -> v : filterMapMaybe f xs

wordList = do
    contents <- readFile "/usr/share/dict/words"
    return $ splitOn "\n" contents

startsWithAny :: Eq a => [a] -> [[a]] -> Bool
startsWithAny s = any (`isPrefixOf` s)

third :: (a, b, c) -> c
third (_, _, c) = c

pairOverlap :: [a] -> [(a, a)]
pairOverlap [] = []
pairOverlap [_] = []
pairOverlap (x1:x2:xs) = (x1, x2) : pairOverlap (x2:xs)

unPair :: (a, a) -> [a]
unPair (a, b) = [a, b]

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a, b) = (f a, f b)

flipPair :: (a, b) -> (b, a)
flipPair (a, b) = (b, a)

makePair :: [a] -> (a, a)
makePair (a:b:_) = (a, b)

makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs (a:b:xs) = (a, b) : makePairs xs

testPair :: (a -> a -> Bool) -> (a, a) -> Bool
testPair f (a, b) = f a b

isPermutation :: String -> String -> Bool
isPermutation a b = sort a == sort b

memoize f = unsafePerformIO $ do
    r <- newIORef Map.empty
    return $ \ x -> unsafePerformIO $ do
        m <- readIORef r
        -- print x
        case Map.lookup x m of
            Just y  -> return y
            Nothing -> do
                    let y = f x
                    writeIORef r (Map.insert x y m)
                    return y

nubOnSorted [] = []
nubOnSorted [a] = [a]
nubOnSorted (a:b:vs)
    | a == b = nubOnSorted (b : vs)
    | otherwise = a : nubOnSorted (b : vs)

mapWhile :: (a -> b) -> (b -> Bool) -> [a] -> [b]
mapWhile f pred = takeWhile pred . map f

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x:xs)
    | f x = x : takeUntil f xs
    | otherwise = [x]

defaultIfNothing _ (Just a) = a
defaultIfNothing def Nothing = def

minimumIndexBy :: Ord b => (a -> b) -> [a] -> (Int, a)
minimumIndexBy f (x:xs) = (i, a)
    where (a, i) = foldl cmpf (x, 0) $ zip xs [1..]
          cmpf (a, ai) (b, bi)
            | f a < f b = (a, ai)
            | otherwise = (b, bi)

minimumIndex :: (Ord a) => [a] -> (Int, a)
minimumIndex = minimumIndexBy id

maximumIndex :: (Ord a) => [a] -> (Int, a)
maximumIndex = maximumIndexBy id

maximumIndexBy :: Ord b => (a -> b) -> [a] -> (Int, a)
maximumIndexBy f (x:xs) = (i, a)
    where (a, i) = foldl cmpf (x, 0) $ zip xs [1..]
          cmpf (a, ai) (b, bi)
            | f a > f b = (a, ai)
            | otherwise = (b, bi)

