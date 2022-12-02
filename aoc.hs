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

-- Day 1
-- main :: IO ()
-- main = readFile "day1input.txt" >>= print . getSum . maximum . map (foldMap $ Sum . read) . splitWhen null . lines
-- main = readFile "day1input.txt" >>= print . getSum . mconcat . take 3 . reverse . sort . map (foldMap $ Sum . read) . splitWhen null . lines

-- Day 2
main :: IO ()
-- main = readFile "day2input.txt" >>= print . sum . map ((\[a, b] -> b-87 + 3 * ((b-a-25)`mod`3)) . map (ord.head) . words) . lines
main = readFile "day2input.txt" >>= print . sum . map ((\[a, b] -> 1 + (a+b-151)`mod`3 + (b-88) * 3) . map (ord.head) . words) . lines