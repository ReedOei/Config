{-# LANGUAGE GADTs #-}

module Library.Math where

import Library.General (takeUntil, pairOverlap, mapPair)
import Library.List (remove, setAt, separateList, groupFromStart)

import Control.Monad.ST

import Data.Array
import Data.Array.ST
import Data.List
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ratio

a `divides` b = b `mod` a == 0

powMod a 0 n = 1
powMod a 1 n = a `mod` n
powMod a b n =
    ((if 2 `divides` b then 1 else a) * powMod (a^2) (b `div` 2) n) `mod` n

heunMethod y' y0 t0 h = scanl next y0 [t0,t0 + h..]
    where next yn t = yn + h / 2 * (y' t yn + y' (t + h) temp)
             where temp = yn + h * y' t yn

eulerMethod y' y0 t0 h = scanl next y0 [t0, t0 + h..]
    where next yn t = yn + h * y' t yn

pascalsTriangle :: Integral a => [[a]]
pascalsTriangle = pascalsTriangle' [1]
    where pascalsTriangle' ns = ns : pascalsTriangle' nextNs
            where nextNs = 1 : map (uncurry (+)) (pairOverlap (ns ++ [1]))

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

approxSlope :: Floating a => (a -> a) -> a -> a -> a
approxSlope f x1 prec = (y2 - y1) / (x2 - x1)
  where y1 = f x1
        x2 = x1 + prec
        y2 = f x2

approxCurveArea :: (Enum a, Floating a) => (a -> a) -> (a, a) -> a -> a
approxCurveArea f (start, end) rects = w * sum ys
  where xs = linSpace rects (start, end)
        ys = map f xs
        w = (xs !! 1) - head xs

linSpace :: (Enum a, Floating a) => a -> (a, a) -> [a]
linSpace num (start, end) = [start,start+increment..end]
  where increment = (end - start) / num

knuthArrow :: Integral a => a -> a -> a -> a
knuthArrow a 1 b = a^b
knuthArrow a depth b = knuthArrow' (b - 1)
    where knuthArrow' 1 = knuthArrow a (depth - 1) a
          knuthArrow' i = knuthArrow a (depth - 1) (knuthArrow' (i - 1))

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

permute :: Integral a => a -> a -> a
n `permute` k = factorial n `div` factorial (n - k)

choose :: Integral a => a -> a -> a
n `choose` k = n `permute` k `div` factorial k

primeCandidates = spin wheel 11
    where wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel
          spin (x:xs) n = n : spin xs (n + x)

findSTArrayIndex :: (Num i, Ix i, MArray a e m) => i -> (e -> Bool) -> m (a i e) -> m (Maybe i)
findSTArrayIndex i f inArr = do
    arr <- inArr
    v <- readArray arr i
    (l, h) <- getBounds arr

    if f v then
        return $ Just i
    else if i >= h then
        return Nothing
    else
        findSTArrayIndex (i + 1) f inArr

-- Constructs a list of prime numbers from 2 to the limit specified.
-- If the limit is greater than 2, only 2 is returned.
-- sieve :: Integer -> [Integer]
{-
sieve n = map fst $ filter snd $ assocs $ runSTArray sieved
    where sieved = sieve' 2 (newArray (2, n) True :: ST s (STArray s Integer Bool))
          sieve' p inArr = do
            arr <- inArr
            (l, h) <- getBounds arr

            mapM_ (\i -> writeArray arr i False) [p*p, p*p+p..n]

            if p*p > h then
                return arr
            else do
                nextP <- findSTArrayIndex (p + 2) id inArr
                case nextP of
                    Nothing -> return arr
                    Just nextP -> sieve' nextP $ return arr
                    -}

-- sieve :: Integral a => a -> [a]
sieve limit = 2 : 3 : 5 : 7 : sieve' (Map.fromList $ zip (takeWhile (<= limit) primeCandidates) (repeat True)) 11
    where sieve' inMap n =
                if nextN^2 < limit then
                    nextN : sieve' nextMap nextN
                else
                    nextN : map fst (filter snd $ Map.toList nextMap)
            where curMap = foldl (flip Map.delete) inMap [n * n, n * n + n..limit]
                  ((nextN, _), nextMap) = Map.deleteFindMin curMap

isSquare n
    | (n `mod` 10) `elem` [2,3,7,8] = False
    | otherwise = floor (sqrt $ fromIntegral n)^2 == n

toDigits :: Integral a => a -> [a]
toDigits n = reverse $ toDigits' n
    where toDigits' 0 = []
          toDigits' v = r : toDigits' d
            where (d, r) = v `divMod` 10

-- factor :: Integer -> [(Integer, Int)]
factor n = factor' n (2:[3,5..])
    where factor' 1 _ = []
          factor' cur [] = [(cur, 1)]
          factor' cur (i:is)
            | i * i > n = [(cur, 1)]
            | count > 0 = (i, count) : factor' nextCur is
            | otherwise = factor' nextCur is
            where (nextCur, count) = getDivCount cur 0
                  getDivCount v count
                    | r == 0 = getDivCount nextV (count + 1)
                    | otherwise = (v, count)
                    where (nextV, r) = v `divMod` i

-- d :: Integral a => a -> Int
d n = product $ map ((+ 1) . snd) $ factor n

-- All stuff for converting to/from arbritary non-integer bases
symbols :: String
symbols = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

getSymbol :: Int -> String
getSymbol n =
    if n < length symbols then
        [symbols !! n]
    else
        "[" ++ show n ++ "]"

data Number a where
    N :: (RealFrac a, Floating a, Enum a) => a -> ([Int], [Int]) -> Number a

instance Show a => Show (Number a) where
    show (N base (whole, decimal)) = wholePart ++ decimalPart ++ "_(" ++ show base ++ ")"
        where wholePart = case whole of
                                [] -> "0"
                                _ -> intercalate "" $ map getSymbol whole
              decimalPart = case decimal of
                                [] -> ""
                                _ -> "." ++ intercalate "" (map getSymbol decimal)

-- Noninteger base functions
fromBaseX :: Number a -> a
fromBaseX (N x (whole,decimal)) = sum wholePart + sum decimalPart
    where wholePart = zipWith (*) (map fromIntegral $ reverse whole) (map (x**) [0..])
          decimalPart = zipWith (*) (map fromIntegral decimal) (map (x**) [-1,-2..])

-- Assumes that indices are passed in sorted (so that a higher index always comes after a lower one)
-- So that we can use lazy evaluation to display stuff
buildDigitList :: (RealFrac a, Num b) => [(a,b)] -> [b]
buildDigitList xs = buildList' xs [] 0
    where buildList' [] cur _ = cur
          buildList' ((i,v):vs) cur curI
            | length cur > floor i = c : buildList' vs nextList (curI + 1)
            | otherwise = l : buildList' vs longList (curI + 1)
            where actualI = floor i - curI
                  (c:nextList) = setAt cur actualI v
                  (l:longList) = cur ++ (replicate (actualI - length cur) 0 ++ [v])

floorFloat :: RealFrac a => a -> a
floorFloat = fromIntegral . floor

-- Returns a tuple where the first list is the whole part of the number, and the second is the decimal part
toBaseX :: (RealFrac a, Floating a, Enum a) => a -> a -> Number a
toBaseX x num = N x (wholeDigits, decimalDigits)
    where wholeDigits = map floor $ reverse $ buildDigitList $ reverse $ init whole
          decimalDigits = buildDigitList $ map (\(a, b) -> (-a - 1, floor b)) $ decimalPart (snd $ last whole) (-1)
          whole = wholePart num
          isValidPower cur p = floorFloat (cur / x**p) < x && cur / x**p >= 1
          wholePart cur
                | cur >= 1 && cur < x = [(0, floorFloat cur), (0, cur - floorFloat cur)] -- Make sure we use the units digit
                | cur < x = [(0, cur)]
                | otherwise = (p, floorFloat (cur / x**p)) : wholePart (cur - x**p * floorFloat (cur / x**p))
              where p = fromMaybe 0 $ find (isValidPower cur) [0..num]
          decimalPart cur prev
                -- If cur is 0, we're done. If p is 0, we probably hit the limit of the precision of the type we're using and it'll just loop forever anyway
                | cur == 0 || p == 0 = []
                | otherwise = (p, floorFloat (cur / x**p)) : decimalPart (cur - x**p * floorFloat (cur / x**p)) p
                where p = fromMaybe 0 $ find (isValidPower cur) [prev,prev - 1..(-10^5)] -- Cap it at 10^5 decimal digits because any more would be absurd
-- End base conversion

-- The following code is all for continued fractions
data ContinuedFraction = ContinuedFraction Integer (Integer -> Integer) Integer

instance Show ContinuedFraction where
    show (ContinuedFraction a f p)
        | p >= 0 = "[" ++ show a ++ ";(" ++ show (map f [0..p - 1]) ++ ")]"
        | otherwise = "[" ++ show a ++ ";(" ++ show (map f [0..5]) ++ ")]"

cfFromList :: [Integer] -> ContinuedFraction
cfFromList (x:xs) = cycleCF x xs

period :: ContinuedFraction -> Integer
period (ContinuedFraction _ _ p) = p

cycleCF :: Integer -> [Integer] -> ContinuedFraction
cycleCF a bs = ContinuedFraction a ((bs !!) . fromIntegral . (`mod` intLen)) intLen
    where intLen = toInteger $ length bs

e :: ContinuedFraction
e = ContinuedFraction 2 e' (-1)
    where e' n = ([1,1] ++ intercalate [1,1] (separateList [2,4..])) !! fromIntegral n

evalCF :: Integer -> ContinuedFraction -> Ratio Integer
evalCF 0 (ContinuedFraction a _ _) = a % 1
evalCF terms (ContinuedFraction a f _) = (a % 1) + 1 / evalCF' 0
    where evalCF' i
            | i < terms = (f i % 1) + 1 / evalCF' (i + 1)
            | otherwise = f i % 1

makeSquareRootCF :: Integer -> ContinuedFraction
makeSquareRootCF n = cfFromList $ squareRootCF n
    where squareRootCF s
            | sqrtS^2 == s = [sqrtS]
            | otherwise = takeUntil (/= 2 * sqrtS) $ triplets (0, 1, sqrtS)
            where triplets (m, d, a) = a : triplets (m_n, d_n, a_n)
                      where m_n = d * a - m
                            d_n = (s - m_n^2) `div` d
                            a_n = (sqrtS + m_n) `div` d_n
                  sqrtS = floor $ sqrt $ fromIntegral s

-- End continued fractions


approximateSquareRoot :: Integer -> [Integer]
approximateSquareRoot s
    | isSquare s = toDigits $ intSqrt s
    | otherwise = firstN : approximateSquareRoot' firstN firstPart firstXs
    where firstN = intSqrt $ fromDigits $ head sDigits
          firstPart = fromDigits (head sDigits) - firstN^2
          firstXs = tail sDigits ++ chunksOf 2 (repeat 0)
          sDigits = groupFromStart 2 $ toDigits s
          approximateSquareRoot' guess part ([x1,x2]:xs) = nextN : approximateSquareRoot' nextGuess (curPart - (2 * guess * 10 + nextN) * nextN) xs
            where nextN = findD (2 * guess) curPart
                  nextGuess = guess * 10 + nextN
                  curPart = part * 100 + x1*10 + x2
                  findD x target = fromMaybe 0 $ find (\i -> i * (x * 10 + i) <= target) [9,8..1]
          approximateSquareRoot' _ _ _ = error "List is empty but shouldn't be."

data Triangle = Triangle (Int, Int) (Int, Int) (Int, Int)
    deriving (Show)

crossProduct2D :: Num a => (a, a) -> (a, a) -> a
crossProduct2D (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

dotProduct :: Num a => [a] -> [a] -> a
dotProduct a b = sum $ zipWith (*) a b

sameSide :: (Int, Int) ->  (Int, Int) ->  (Int, Int) ->  (Int, Int) -> Bool
sameSide (p1x,p1y) (p2x,p2y) (ax,ay) (bx,by) = dotProduct [cp1] [cp2] >= 0
    where cp1 = crossProduct2D (bx - ax, by - ay) (p1x - ax, p1y - ay)
          cp2 = crossProduct2D (bx - ax, by - ay) (p2x - ax, p2y - ay)

pointIsInTriangle :: (Int, Int) -> Triangle -> Bool
pointIsInTriangle p (Triangle a b c) = sameSide p a b c && sameSide p b a c && sameSide p c a b

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

pentagonal n = n * (3 * n - 1) `div` 2
pentagonalNumbers limit = takeWhile (<= limit) (map pentagonal znonzero)

n = [1..]

z = 0 : [y | n <- [1..], y <- [n, -n]]

znonzero = tail z

points = [(x, y) | n <- [1..], x <- take n z, y <- take n z]

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

partialSums :: Num a => [a] -> [a]
partialSums = partialSums' 0
    where partialSums' i [] = [i]
          partialSums' i (x:xs) = i + x : partialSums' (i + x) xs

truncates :: Integral a => a -> [a]
truncates n = truncates' $ init $ reverse $ digits n
    where truncates' [] = []
          truncates' xs = fromDigits xs : truncates' (init xs)

toCartesian :: PolarPoint -> Point
toCartesian (r, theta) = (r * cos theta, r * sin theta)

toPolar :: Point -> PolarPoint
toPolar (x, y) = (sqrt (x^2 + y^2), atan (y / x))
sumPair :: Num a => (a, a) -> (a, a) -> (a, a)
(a1, b1) `sumPair` (a2, b2) = (a1 + a2, b1 + b2)

distance :: (Integral a, Floating b) => (a, a) -> (a, a) -> b
distance (x1, y1) (x2, y2) = sqrt $ (x1f - x2f)**2 + (y1f - y2f)**2
    where (x1f, y1f) = (fromIntegral x1, fromIntegral y1)
          (x2f, y2f) = (fromIntegral x2, fromIntegral y2)

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

allRatios :: Integral a => [a] -> [Ratio a]
allRatios ns = concatMap(\a -> map (% a) ns) $ filter (/= 0) ns

averagePairs :: Floating a => [(a, a)] -> (a, a)
averagePairs ps = (a / len, b / len)
    where (a, b) = sumPairs ps
          len = fromIntegral $ length ps

pairRatio :: Integral a => (a, a) -> Ratio a
pairRatio (a, b) = a % b

sumPairs :: (Num a, Num b) => [(a, b)] -> (a, b)
sumPairs [] = (0, 0)
sumPairs [(a,b)] = (a,b)
sumPairs ((a, b):xs) = (a + na, b + nb)
    where (na, nb) = sumPairs xs

