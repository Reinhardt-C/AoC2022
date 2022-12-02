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
main = day2b

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
