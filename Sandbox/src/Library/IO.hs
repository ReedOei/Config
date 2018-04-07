module Library.IO (noBuffer,
                   printFilterMap,
                   showMaxBy, showMinBy,
                   showProgress, showProgressZipped) where
import System.IO
import System.ProgressBar

noBuffer :: IO ()
noBuffer = hSetBuffering stdout NoBuffering

printFilterMap :: (a -> Bool) -> (a -> String) -> (a -> String) -> [a] -> IO ()
printFilterMap f trueS falseS = mapM_ printFilterMap'
    where printFilterMap' v
            | f v = putStrLn $ "\r" ++ trueS v
            | otherwise = putStr $ "\r" ++ falseS v

showMaxBy :: (Show a, Ord b) => (a -> b) -> [a] -> IO a
showMaxBy _ [] = error "Empty list."
showMaxBy f (v:vs) = do
    print v
    showMaxBy' v vs
    where showMaxBy' cur [] = return cur
          showMaxBy' cur (x:xs)
            | f x > f cur = do
                print x
                showMaxBy' x xs
            | otherwise = showMaxBy' cur xs

showMinBy :: (Show a, Ord b) => (a -> b) -> [a] -> IO a
showMinBy _ [] = error "Empty list."
showMinBy f (v:vs) = do
    print v
    showMinBy' v vs
    where showMinBy' cur [] = return cur
          showMinBy' cur (x:xs)
            | f x < f cur = do
                print x
                showMinBy' x xs
            | otherwise = showMinBy' cur xs

showProgressZipped limit = mapM_ (\(i, r) -> progressBar (msg (show i ++ " of " ++ show limit ++ " (" ++ show r ++ ")")) percentage 80 (Progress i limit))
showProgress limit = mapM_ (\i -> progressBar (msg (show i ++ " of " ++ show limit)) percentage 80 (Progress i limit))
