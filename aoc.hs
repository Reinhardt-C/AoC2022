-- {-# LANGUAGE TypeApplications #-}
module Main where

import System.IO
import Control.Monad
import Data.Monoid
import Data.List
import Data.List.Split
import Data.Function
import Data.Coerce
import Data.Ord
import Data.Char
import Data.Map as Map (Map, fromList, empty, insert, adjustWithKey, mapWithKey, toList)
import qualified Data.Map as Map (filter)

main :: IO ()
main = day7b

-- Day 7
day7a :: IO ()
day7a = readFile "day7input.txt" >>= print . sum . filter (<=100000) . map (\(_, Dir _ s) -> s)
                                     . toList . getStructure . (\f -> f initState) . appEndo . getDual . foldMap (Dual . Endo . lineToFunc) . lines
day7b :: IO ()
day7b = readFile "day7input.txt" >>= print . snd . minimumBy (compare `on` snd) . (\ps@(p:_) -> filter (\(_, s) -> s>=snd p - 40000000) ps) . map (\(d, Dir _ s) -> (head $ splitOn "/" d, s))
                                     . toList . getStructure . (\f -> f initState) . appEndo . getDual . foldMap (Dual . Endo . lineToFunc) . lines

initState :: FSState
initState = State [] (Map.fromList [("/", emptyDir)])

emptyDir :: FSDir
emptyDir = Dir [] 0

lineToFunc :: [Char] -> FSState -> FSState
lineToFunc l
    | "$ cd " `isPrefixOf` l    = cd (drop 5 l)
    | "dir " `isPrefixOf` l     = dir (drop 4 l)
    | "$ ls" == l               = id
    | otherwise                 = \(State p m) -> State p $ (appEndo . foldMap (Endo . adjustWithKey (\_ (Dir ds s) -> Dir ds (s + read ((head . words) l))) . intercalate "/")) ((init . tails) p) m

cd :: [Char] -> FSState -> FSState
cd ".." (State p m) = State (tail p) m
cd d (State p m) = State (d:p) m

dir :: String -> FSState -> FSState
dir d (State p m) = State p (adjustWithKey (\_ (Dir ds s) -> Dir (d:ds) s) (intercalate "/" p) $ Map.insert (intercalate "/" (d:p)) emptyDir m)

data FSDir = Dir {getChildDirs :: [String], getSize :: Int} deriving (Show)
data FSState = State {getPath :: [String], getStructure :: Map String FSDir} deriving (Show)

-- Day 6
day6a :: IO ()
day6a = readFile "day6input.txt" >>= print . maybe (-1) (+4) . findIndex (\s -> s==nub s) . map (take 4) . tails
day6b :: IO ()
day6b = readFile "day6input.txt" >>= print . maybe (-1) (+14) . findIndex (\s -> s==nub s) . map (take 14) . tails

-- Day 5
day5a :: IO ()
day5a = readFile "day5input.txt" >>= print . map head . (\[a,b] -> (appEndo . getDual . foldMap (Dual . Endo . runInstr) $ parseInstr b) $ parsePiles a) . splitWhen null . lines
    where
        parsePiles = map (filter (/=' ') . map (!!1)) . transpose . init . map (chunksOf 4)
        parseInstr = map (map (flip (-) 1 . read) . last . transpose . chunksOf 2 . words)
        runInstr [n,a,b] piles
            | a<=b  = take a piles ++ [newA] ++ (drop (a+1) . take b) piles ++ [newB] ++ drop (b+1) piles
            | b<a   = take b piles ++ [newB] ++ (drop (b+1) . take a) piles ++ [newA] ++ drop (a+1) piles
            where
                newA = drop (n+1) (piles !! a)
                newB = reverse (take (n+1) (piles !! a)) ++ piles !! b
        runInstr _ _ = []
day5b :: IO ()
day5b = readFile "day5input.txt" >>= print . map head . (\[a,b] -> (appEndo . getDual . foldMap (Dual . Endo . runInstr) $ parseInstr b) $ parsePiles a) . splitWhen null . lines
    where
        parsePiles = map (filter (/=' ') . map (!!1)) . transpose . init . map (chunksOf 4)
        parseInstr = map (map (flip (-) 1 . read) . last . transpose . chunksOf 2 . words)
        runInstr [n,a,b] piles
            | a<=b  = take a piles ++ [newA] ++ (drop (a+1) . take b) piles ++ [newB] ++ drop (b+1) piles
            | b<a   = take b piles ++ [newB] ++ (drop (b+1) . take a) piles ++ [newA] ++ drop (a+1) piles
            where
                newA = drop (n+1) (piles !! a)
                newB = take (n+1) (piles !! a) ++ piles !! b
        runInstr _ _ = []

-- Day 4
day4a :: IO ()
day4a = readFile "day4input.txt" >>= print . length . filter (\[a,b,c,d] -> a<=c&&b>=d || a>=c&&b<=d) . map (map (read :: String -> Int) . splitOneOf ",-") . lines
day4b :: IO ()
day4b = readFile "day4input.txt" >>= print . length . filter (\[a,b,c,d] -> a<=d&&b>=c || a>=d&&b<=c) . map (map (read :: String -> Int) . splitOneOf ",-") . lines

-- Day 3
day3a :: IO ()
day3a = readFile "day3input.txt" >>= print . sum . map ((\x -> if x>=97 then x-96 else x-38) . ord . head . nub . uncurry intersect . (\xs -> splitAt (length xs `div` 2) xs)) . lines
day3b :: IO ()
day3b = readFile "day3input.txt" >>= print . sum . map ((\x -> if x>=97 then x-96 else x-38) . ord . head . nub . foldr1 intersect) . chunksOf 3 . lines

-- Day 2
day2a :: IO ()
day2a = readFile "day2input.txt" >>= print . sum . map ((\[a, b] -> b-87 + 3 * ((b-a-25)`rem`3)) . map (ord.head) . words) . lines
day2b :: IO ()
day2b = readFile "day2input.txt" >>= print . sum . map ((\[a, b] -> 1 + (a+b-151)`rem`3 + (b-88) * 3) . map (ord.head) . words) . lines

-- Day 1
day1a :: IO ()
day1a = readFile "day1input.txt" >>= print . getSum . maximum . map (foldMap $ Sum . read) . splitWhen null . lines
day1b :: IO ()
day1b = readFile "day1input.txt" >>= print . getSum . mconcat . take 3 . reverse . sort . map (foldMap $ Sum . read) . splitWhen null . lines
