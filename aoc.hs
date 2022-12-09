{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Map as Map (Map, empty, insert, adjustWithKey, mapWithKey, toList, member)
import qualified Data.Map as Map (fromList, filter, lookup)
import Data.Maybe
import GHC.Float
import Data.Set (size)
import qualified Data.Set as Set (fromList)

main :: IO ()
main = day9b

-- Day 9
day9a :: IO ()
day9a = readFile "day9input.txt" >>= print . size . Set.fromList . (\(_,_,h)->h) . (\f -> f ([0, 0], [0, 0], [])) . appEndo . getDual . foldMap (Dual . Endo . move . direction)
                                     . foldr1 (++) . map ((\[a, b] -> replicate (read b) a) . words) . lines
day9b :: IO ()
day9b = readFile "day9input.txt" >>= print . size . Set.fromList . snd . (\f -> f (replicate 10 [0, 0], [])) . appEndo . getDual . foldMap (Dual . Endo . moveStep . direction)
                                     . foldr1 (++) . map ((\[a, b] -> replicate (read b) a) . words) . lines

directions :: Map [Char] [Int]
directions = Map.fromList [("R", [1, 0]), ("L", [-1, 0]), ("U", [0, 1]), ("D", [0, -1])]
direction :: [Char] -> [Int]
direction x
    | x `member` directions = fromMaybe [0, 0] (Map.lookup x directions)
    | otherwise             = [0, 0]
move :: [Int] -> ([Int], [Int], [[Int]]) -> ([Int], [Int], [[Int]])
move [dx, dy] ([tx, ty], [hx, hy], hs) = (nt, [hx + dx, hy + dy], hs ++ [nt])
    where nt = if max (abs $ hx + dx - tx) (abs $ hy + dy - ty) <= 1 then [tx, ty] else [tx + clamp 1 (hx + dx - tx), ty + clamp 1 (hy + dy - ty)]
move _ _ = ([], [], [])

moveIndex :: Int -> ([[Int]], [[Int]]) -> ([[Int]], [[Int]])
moveIndex i (ks, hs) = (take (i+1) ks ++ [x] ++ drop (i+2) ks, z)
    where (x, y, z) = move [0, 0] (ks!!(i+1), ks!!i, hs)

moveStep :: [Int] -> ([[Int]], [[Int]]) -> ([[Int]], [[Int]])
moveStep [dx, dy] ([hx, hy]:ks, hs) = (appEndo . getDual . foldMap (Dual . Endo . (. (\(a,b) -> (a,hs))) . moveIndex) $ [0..length ks - 1]) ([hx+dx, hy+dy]:ks, hs)
moveStep _ _ = ([], [])

-- Day 8
day8a :: IO ()
day8a = readFile "day8input.txt" >>= print . length . filter id . foldr1 (++) . foldr1 (zipWith $ zipWith (||))
                                     . (\(a, b, c, d) -> [checkLine last a, transpose $ checkLine last b, checkLine head c, transpose $ checkLine head d])
                                     . (\arr -> (map (tail.inits) arr, map (tail.inits) . transpose $ arr, map (init.tails) arr, map (init.tails) . transpose $ arr))
                                     . map (map $ read @Int . (:[])) . lines
    where checkLine f = (map . map) (\x -> maximum ((-1):delete (f x) x) < f x)
day8b :: IO ()
day8b = readFile "day8input.txt" >>= print . maximum . map maximum .  foldr1 (zipWith $ zipWith (*))
                                     . (\(a, b, c, d) -> [checkLine reverse a, transpose $ checkLine reverse b, checkLine id c, transpose $ checkLine id d])
                                     . (\arr -> (map (tail.inits) arr, map (tail.inits) . transpose $ arr, map (init.tails) arr, map (init.tails) . transpose $ arr))
                                     . map (map $ read @Int . (:[])) . lines
    where checkLine f = (map . map) ((\(x:xs) -> if all (<x) xs then length xs else (+1) . length $ takeWhile (<x) xs) . f)

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

lineToFunc :: [Char] -> (FSState -> FSState)
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
day5b :: IO ()
day5b = readFile "day5input.txt" >>= print . map head . (\[a,b] -> (appEndo . getDual . foldMap (Dual . Endo . runInstr) $ parseInstr b) $ parsePiles a) . splitWhen null . lines

parsePiles :: [[Char]] -> [[Char]]
parsePiles = map (filter (/=' ') . map (!!1)) . transpose . init . map (chunksOf 4)
parseInstr :: [String] -> [[Int]]
parseInstr = map (map (flip (-) 1 . read) . last . transpose . chunksOf 2 . words)
runInstr :: [Int] -> [[a]] -> [[a]]
runInstr [n,a,b] piles
    | a<=b  = take a piles ++ [newA] ++ (drop (a+1) . take b) piles ++ [newB] ++ drop (b+1) piles
    | b<a   = take b piles ++ [newB] ++ (drop (b+1) . take a) piles ++ [newA] ++ drop (a+1) piles
    where
        newA = drop (n+1) (piles !! a)
        newB = take (n+1) (piles !! a) ++ piles !! b
runInstr _ _ = []

-- Day 4
day4a :: IO ()
day4a = readFile "day4input.txt" >>= print . length . filter (\[a,b,c,d] -> a<=c&&b>=d || a>=c&&b<=d) . map (map (read @Int) . splitOneOf ",-") . lines
day4b :: IO ()
day4b = readFile "day4input.txt" >>= print . length . filter (\[a,b,c,d] -> a<=d&&b>=c || a>=d&&b<=c) . map (map (read @Int) . splitOneOf ",-") . lines

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
