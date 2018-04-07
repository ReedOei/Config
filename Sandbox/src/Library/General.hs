module Library.General
        (memoize,
         n, z, znonzero,
         mapPair, flipPair, pairRatio, makePair, makePairs, testPair, pairOverlap, unPair, sumFirst, sumSecond, sumPairs, sumPair, eitherInPair,
         third,
         wordList,
         sumDigits, digits, fromDigits,
         pentagonal, pentagonalNumbers, triangleNumber, triangleN,
         quadratic, intQuadratic,
         nubOnSorted, mapWhile, scanlWhile, takeUntil, collectUntil, collectWhile,
         defaultIfNothing,
         startsWithAny,
         differences, ratios, ratioTo, evalRatio,
         isPermutation,
         truncates,
         allProducts,
         incrementAt, incrementDigitsIf, incrementDigitsToIf,
         minimumIndex, maximumIndex, minimumIndexBy, maximumIndexBy,
         distance,
         filterMapMaybe,
         isInRange, clamp, wrap,
         toPolar, toCartesian,
         findIndexVal,
         averageFirst, averageSecond, averagePairs, average,
         roughMatch, percentMatch,
         findMaybe, findAllMaybe,
         triples,
         allRatios) where

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

allRatios :: Integral a => [a] -> [Ratio a]
allRatios ns = concatMap(\a -> map (% a) ns) $ filter (/= 0) ns

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

type Point = (Float, Float) -- (x, y)
type PolarPoint = (Float, Float) -- (r, theta)

average xs = realToFrac (sum xs) / genericLength xs

averageFirst :: Floating a => [(a, b)] -> (a, b)
averageFirst xs = (a / len, b)
    where (a, b) = sumFirst xs
          len = fromIntegral $ length xs

averageSecond :: Floating a => [(b, a)] -> (b, a)
averageSecond xs = (a, b / len)
    where (a, b) = sumSecond xs
          len = fromIntegral $ length xs

sumFirst :: Num a => [(a, b)] -> (a, b)
sumFirst [(a,b)] = (a, b)
sumFirst ((a, b):xs) = (a + sa, b)
    where (sa, _) = sumFirst xs

sumSecond :: Num a => [(b, a)] -> (b, a)
sumSecond [(a,b)] = (a, b)
sumSecond ((a, b):xs) = (a, b + sb)
    where (_, sb) = sumSecond xs

averagePairs :: Floating a => [(a, a)] -> (a, a)
averagePairs ps = (a / len, b / len)
    where (a, b) = sumPairs ps
          len = fromIntegral $ length ps

findIndexVal :: (a -> Bool) -> [a] -> Maybe (Int, a)
findIndexVal f xs = find (f . snd) $ zip [0..] xs

toCartesian :: PolarPoint -> Point
toCartesian (r, theta) = (r * cos theta, r * sin theta)

toPolar :: Point -> PolarPoint
toPolar (x, y) = (sqrt (x^2 + y^2), atan (y / x))

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

distance :: (Integral a, Floating b) => (a, a) -> (a, a) -> b
distance (x1, y1) (x2, y2) = sqrt $ (x1f - x2f)**2 + (y1f - y2f)**2
    where (x1f, y1f) = (fromIntegral x1, fromIntegral y1)
          (x2f, y2f) = (fromIntegral x2, fromIntegral y2)

sumPair :: Num a => (a, a) -> (a, a) -> (a, a)
(a1, b1) `sumPair` (a2, b2) = (a1 + a2, b1 + b2)

wordList = do
    contents <- readFile "/usr/share/dict/words"
    return $ splitOn "\n" contents

incrementAt :: Integral a => [a] -> Int -> [a]
incrementAt xs i = take i xs ++ [e + 1] ++ drop (i + 1) xs
    where e = xs !! i

incrementDigitsToIf :: Integral a => a -> ([a] -> Bool) -> [a] -> [a]
incrementDigitsToIf v f ns = incrementDigitsToIf' ns 0
    where incrementDigitsToIf' ds i
            | i >= length ds = ds ++ [0]
            | f (incrementAt ds i) = incrementAt ds i
            | otherwise = incrementDigitsToIf' (setAt ds i v) (i + 1)

incrementDigitsIf :: Integral a => ([a] -> Bool) -> [a] -> [a]
incrementDigitsIf = incrementDigitsToIf 0

allProducts :: (Ord a, Num a) => a -> [a] -> [a]
allProducts _ [] = []
allProducts limit (n:ns) = allProducts' (n:ns) ++ allProducts limit ns
    where allProducts' [] = []
          allProducts' (x:xs)
            | n * x > limit = []
            | otherwise = n * x : allProducts' xs

startsWithAny :: Eq a => [a] -> [[a]] -> Bool
startsWithAny s = any (`isPrefixOf` s)

partialSums :: Num a => [a] -> [a]
partialSums = partialSums' 0
    where partialSums' i [] = [i]
          partialSums' i (x:xs) = i + x : partialSums' (i + x) xs

truncates :: Integral a => a -> [a]
truncates n = truncates' $ init $ reverse $ digits n
    where truncates' [] = []
          truncates' xs = fromDigits xs : truncates' (init xs)

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

pairRatio :: Integral a => (a, a) -> Ratio a
pairRatio (a, b) = a % b

sumPairs :: (Num a, Num b) => [(a, b)] -> (a, b)
sumPairs [] = (0, 0)
sumPairs [(a,b)] = (a,b)
sumPairs ((a, b):xs) = (a + na, b + nb)
    where (na, nb) = sumPairs xs

makePair :: [a] -> (a, a)
makePair (a:b:_) = (a, b)

makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs (a:b:xs) = (a, b) : makePairs xs

testPair :: (a -> a -> Bool) -> (a, a) -> Bool
testPair f (a, b) = f a b

isPermutation :: String -> String -> Bool
isPermutation a b = sort a == sort b

fromDigits :: Num a => [a] -> a
fromDigits = foldl (\a b -> 10 * a + b) 0

digits :: Integral a => a -> [a]
digits = digits'
    where digits' n
            | n < 10 = [n]
            | otherwise = m : digits' d
            where (d, m) = n `quotRem` 10

sumDigits :: Integral a => a -> a
sumDigits = sum . digits

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

pentagonal n = n * (3 * n - 1) `div` 2
pentagonalNumbers limit = takeWhile (<= limit) (map pentagonal znonzero)

n = [1..]

z = 0 : [y | n <- [1..], y <- [n, -n]]

znonzero = tail z

points = [(x, y) | n <- [1..], x <- take n z, y <- take n z]

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

differences :: Num a => [a] -> [a]
differences [a,b] = [b - a]
differences (a:b:bs) = (b - a) : differences (b : bs)
differences _ = []

ratioTo :: Integral a => a -> [a] -> [Ratio a]
ratioTo test [a] = [a % test]
ratioTo test (a:as) = (a % test) : ratioTo test as

ratios :: Integral a => [a] -> [Ratio a]
ratios [a,b] = [b % a]
ratios (a:b:bs) = (b % a) : ratios (b : bs)
ratios _ = []

evalRatio :: (Integral a, Fractional b) => Ratio a -> b
evalRatio f = fromIntegral (numerator f) / fromIntegral (denominator f)

triangleNumber :: Integral a => a -> a
triangleNumber n = n * (n + 1) `div` 2

triangleN :: Integral a => a -> a
triangleN n = fst $ intQuadratic 1 1 ((-2) * n)

intQuadratic :: Integral a => a -> a -> a -> (a, a)
intQuadratic a b c = mapPair round $ quadratic a' b' c'
    where a' = fromIntegral a
          b' = fromIntegral b
          c' = fromIntegral c

quadratic :: (RealFrac a, Floating a) => a -> a -> a -> (a, a)
quadratic a b c = (x0, x1)
    where x0 = ((-b) + sqrt (b**2 - 4*a*c)) / (2 * a)
          x1 = ((-b) - sqrt (b**2 - 4*a*c)) / (2 * a)

