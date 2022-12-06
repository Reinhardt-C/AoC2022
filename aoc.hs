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

main :: IO ()
main = day6b

-- Day 6
day6a :: IO ()
day6a = readFile "day6input.txt" >>= print . maybe (-1) (+4) . findIndex (\s -> s==nub s) . (\ds -> map (map (ds!!) . (\x -> [x..x+3])) [0..length ds-4])
day6b :: IO ()
day6b = readFile "day6input.txt" >>= print . maybe (-1) (+14) . findIndex (\s -> s==nub s) . (\ds -> map (map (ds!!) . (\x -> [x..x+13])) [0..length ds-14])

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
                newB = reverse (take (n+1) (piles !! a)) ++ (piles !! b)
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
                newB = take (n+1) (piles !! a) ++ (piles !! b)
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
