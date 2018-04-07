module Library.List ((!!!),
                     binarySearch,
                     inMatrix,
                     allCombinations,
                     getMatchPos, getMatchPos2,
                     crop, reshape,
                     diagonals,
                     neighbors,
                     combinations, combinationElements, allSubsequences, sequences,
                     groupOverlap, groupFromStart,
                     halve,
                     removeAt, remove, removeAll, replaceInfix,
                     setAt, setAt2,
                     dims, width, height,
                     padL, padR,
                     separateList,
                     intersperseBy,
                     count, countDuplicates, countDuplicatesBy, unduplicate,
                     zipTo) where
import Data.List (inits, tails)

zipTo :: (a -> b) -> [a] -> [(a, b)]
zipTo _ [] = []
zipTo f (x:xs) = (x, f x) : zipTo f xs

groupFromStart :: Int -> [a] -> [[a]]
groupFromStart len l@(x:xs)
    | length l `mod` 2 > 0 = [x] : groupFromStart' xs
    | otherwise = groupFromStart' l
    where groupFromStart' [] = []
          groupFromStart' xs = take len xs : groupFromStart' (drop len xs)

unduplicate :: Integral a => [(a,b)] -> [b]
unduplicate [] = []
unduplicate ((n,v):vs) = replicate (fromIntegral n) v ++ unduplicate vs

count :: Num b => (a -> Bool) -> [a] -> b
count _ [] = 0
count f (x:xs)
    | f x = 1 + count f xs
    | otherwise = count f xs

countDuplicates :: (Eq a, Num b) => [a] -> [(b, a)]
countDuplicates = countDuplicatesBy id

countDuplicatesBy :: (Eq a, Eq b, Num c) => (a -> b) -> [a] -> [(c, b)]
countDuplicatesBy _ [] = []
countDuplicatesBy f (x:xs) = (count (\i -> f i == f x) xs + 1, f x) : countDuplicatesBy f (filter (\i -> f i /= f x) xs)

intersperseBy _ [] = []
intersperseBy f (l:ls) = l ++ [f (last l)] ++ intersperseBy f ls

isInRange :: Ord a => a -> (a, a) -> Bool
isInRange v (a, b) = a <= v && v < b

binarySearch :: (Ord a) => a -> [a] -> Bool
binarySearch _ [] = False
binarySearch a [b] = a == b
binarySearch a xs
    | a == head secondHalf = True
    | a > head secondHalf = binarySearch a secondHalf
    | otherwise = binarySearch a firstHalf
    where (firstHalf, secondHalf) = halve xs

(!!!) :: Integral b => [[a]] -> (b, b) -> a
xss !!! (x', y') = (xss !! y) !! x
    where (x, y) = (fromIntegral x', fromIntegral y')

inMatrix :: (Int, Int) ->  [[a]] -> Bool
inMatrix (x, y) matrix
    | x < 0 || y < 0 = False
    | h <= y = False
    | w <= x = False
    | otherwise = True
    where (w, h) = dims matrix

allCombinations :: Eq a => [a] -> [[a]]
allCombinations ns = concatMap (`combinations` ns) [1..length ns]

getMatchPos :: Eq a => [a] -> [a] -> Maybe Int
getMatchPos match check = getMatchPos' check 0
    where getMatchPos' [] _ = Nothing
          getMatchPos' c@(_:cs) i
            | match == take ml c = Just i
            | otherwise = getMatchPos' cs (i + 1)
          ml = length match

getMatchPos2 :: (Show a, Eq a) => [[a]] -> [[a]] -> Maybe (Int, Int)
getMatchPos2 match@(m1:ms) check
    | cw < mw || ch < mh = Nothing
    | otherwise = getMatchPos' check 0
    where getMatchPos' [] _ = Nothing
          getMatchPos' c@(c1:cs) y = case getMatchPos m1 c1 of
                                        Just x -> if map (take mw . drop x) (take mh c) == match then
                                                    Just (x, y)
                                                  else
                                                    getMatchPos' cs (y + 1)
                                        Nothing -> getMatchPos' cs (y + 1)
          (mw, mh) = dims match
          (cw, ch) = dims check

crop :: (Int, Int) -> (Int, Int) -> [[a]] -> [[a]]
crop (xmin, xmax) (ymin, ymax) img = final
    where final = map (take (xmax - xmin) . drop xmin) ys
          ys = take (ymax - ymin) $ drop ymin img

combinations :: Eq a => Int -> [a] -> [[a]]
combinations 1 l = separateList l
combinations n l = [x : t | x:xs <- tails l, t <- combinations (n - 1) xs]

reshape :: Int -> [a] -> [[a]]
reshape w xs
    | length xs < w = []
    | otherwise = take w xs : reshape w (drop w xs)

diagonals :: [[a]] -> [[a]]
diagonals xss = [diag1, reverse diag2]
    where (w, h) = dims xss
          diag1 = [xss !!! (x, y) | x <- [0..w - 1], let y = x]
          diag2 = [xss !!! (x, y) | x <- [0..w - 1], let y = h - x - 1]

neighbors :: [[a]] -> (Int, Int) -> [a]
neighbors rows (x, y) = [rows !!! (x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1],
                                             dx /= 0 || dy /= 0,
                                             (x + dx) `isInRange` (0, w),
                                             (y + dy) `isInRange` (0, h)]
     where (w, h) = dims rows

allSubsequences :: [a] -> [[a]]
allSubsequences [] = []
allSubsequences [x] = [[x]]
allSubsequences x@(_:xs) = subsequences' x ++ allSubsequences xs
    where subsequences' as = tail $ inits as

groupOverlap :: Int -> [a] -> [[a]]
groupOverlap _ [] = []
groupOverlap len xs = take len xs : groupOverlap len (tail xs)

halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve xs = go xs xs
    where go (x:xs) (_:_:ys) = let (first,last) = go xs ys in (x:first, last)
          go (x:xs) [_] = ([x],xs)
          go (x:xs) []  = ([],x:xs)

removeAt :: Int -> [a] -> [a]
removeAt i xs = take (i - 1) xs ++ drop i xs

remove :: Eq a => [a] -> a -> [a]
remove [] _ = []
remove (x:xs) e
    | x == e = xs
    | otherwise = x : remove xs e

removeAll :: Eq a => [a] -> a -> [a]
removeAll xs e = filter (/= e) xs

setAt :: [a] -> Int -> a -> [a]
setAt xs i e = take i xs ++ [e] ++ drop (i + 1) xs

setAt2 :: [[a]] -> (Int, Int) -> a -> [[a]]
setAt2 xs (x, y) e = take y xs ++ [setAt (xs !! y) x e] ++ drop (y + 1) xs

dims :: [[a]] -> (Int, Int)
dims xs = (length (head xs), length xs)

width :: [[a]] -> Int
width = fst . dims

height :: [[a]] -> Int
height = snd . dims

padL :: a -> Int -> [a] -> [a]
padL e i xs
    | length xs >= i = xs
    | otherwise = padL e i (e : xs)

padR :: a -> Int -> [a] -> [a]
padR e i xs
    | length xs >= i = xs
    | otherwise = padR e i (xs ++ [e])

separateList :: [a] -> [[a]]
separateList = map (:[])

combinationElements :: [[a]] -> [[a]]
combinationElements [x] = [[i] | i <- x]
combinationElements (x:xs) = [i : nc | i <- x, nc <- combinationElements xs]

sequences :: Integral b => [a] -> b -> [[a]]
sequences xs 1 = separateList xs
sequences xs i = [x : s | x <- xs, s <- sequences xs (i - 1)]

replaceInfix :: Eq a => [a] -> [a] -> [a] -> [a]
replaceInfix _ _ [] = []
replaceInfix cs es xs@(x:nextXs)
    | take (length cs) xs == cs = es ++ drop (length cs) xs
    | otherwise = x : replaceInfix cs es nextXs

