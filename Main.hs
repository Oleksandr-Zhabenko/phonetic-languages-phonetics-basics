-- |
-- Module      :  Main
-- Copyright   :  (c) Oleksandr Zhabenko 2020-2023
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Can be used to calculate the durations of the approximations of the phonemes
-- using some prepared text with its correct (at least mostly) pronunciation.
-- The prepared text is located in the same directory and contains lines -the
-- phonetic language word and its duration in seconds separated with whitespace.
-- The library is intended to use the functionality of the :
-- 
-- 1) R programming language https://www.r-project.org/
-- 
-- 2) Rglpk library https://cran.r-project.org/web/packages/Rglpk/index.html
-- 
-- 3) GNU GLPK library https://www.gnu.org/software/glpk/glpk.html
-- 
-- For more information, please, see the documentation for them.
-- 
-- For the model correctness the js here refers to sorted list of the 'Char' representations of the phonetic language phenomenae.
-- 
-- The length of the 'String' js is refered to as 'lng'::'Int'. The number of 'pairs'' function elements in the lists is refered to
-- as 'nn'::'Int'. The number of constraints is refered here as 'nc'::'Int'. @nc == nn `quot` 2@.
-- 
-- Is generalized from the Phladiprelio.RGLPK.Ukrainian module from
-- the @r-glpk-phonetic-languages-ukrainian-durations@ package.

{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import GHC.Base
import GHC.Real ((/))
import GHC.Float (sqrt)
import GHC.Num ((*),(-),abs)
import Data.Char (isAlpha)
import Phladiprelio.RGLPK.General
import System.Environment (getArgs)
import GHC.Arr
import Text.Read
import Data.List
import GHC.Int 
import Data.Maybe (fromMaybe,fromJust)
import Phladiprelio.General.Base
import Phladiprelio.General.SpecificationsRead
import System.IO

main :: IO ()
main = do
 args <- getArgs
 let min1 = - abs (fromMaybe (-0.003) (readMaybe (concat . take 1 $ args)::Maybe Double))
     max1 = abs (fromMaybe 0.003 (readMaybe (concat . drop 1. take 2 $ args)::Maybe Double))
     min2 = - abs (fromMaybe (-0.0012) (readMaybe (concat . drop 2 . take 3 $ args)::Maybe Double))
     max2 = abs (fromMaybe 0.0012 (readMaybe (concat . drop 3 . take 4 $ args)::Maybe Double))
     arGs = dropWhile (all (not . isAlpha)) args
 processMentG arGs min1 max1 min2 max2

processMentG :: [String] -> Double -> Double -> Double -> Double -> IO ()
processMentG ts min1 max1 min2 max2 = do
  let file = concat . take 1 $ ts
      controlFile = concat . drop 1 . take 2 $ ts
      gwritingsysFile = concat . drop 2 . take 3 $ ts
  contents <- readFile file
  controlConts <- readFile controlFile
  gwrsCnts <- readFile gwritingsysFile
  let [pwHeader, objCfs, xs1sps1s] = groupBetweenChars '~' . lines $ controlConts
      gwrs = getGWritingSystem '~' gwrsCnts
      nn = fromMaybe 1 (readMaybe (concat . take 1 $ pwHeader)::Maybe Int)
      pw = LL (map (\zzs -> read zzs::PairwisePL) . drop 1 $ pwHeader) nn
      gs = fromMaybe [] (readMaybe (concat objCfs)::Maybe [(Int,Int)])
      xs1 = fromMaybe [] (readMaybe (concat . take 1 $ xs1sps1s)::Maybe [Int])
      sps1 = fromMaybe [] (readMaybe (concat . drop 1 . take 2 $ xs1sps1s)::Maybe [Int])
      [mx,mn1,mnSpecial,mnG] = map (\s -> fromJust (readMaybe s::Maybe Double)) . drop 3 . take 7 $ ts
      coeff = fromMaybe (sqrt 2.0) ((readMaybe (concat . drop 7 . take 8 $ ts))::Maybe Double)
      lst0 = createCoeffsObj lng (drop 7 ts)
      ll = length lst0 - 1
      lstCfs = listArray (0,ll) lst0
      xss = map words . lines $ contents
      words2 = map head xss
      lengths0 = map ((\rs -> read rs::Double) . last) xss
      bss = map (sort . map char . stringToXG gwrs) words2
      js = tail . nub . sort . unwords $ bss
      lng = length js
  putStrLn . answer2 lng nn pw mx gs min1 max1 min2 max2 mn1 mnSpecial mnG xs1 sps1 {- <---  new coeffs-} lstCfs bss (map (*coeff) lengths0) (map (* (1.0 / coeff)) lengths0) $ js
              
-- Example of usage: pldPL 0.001 0.001 0.01 0.02 words.txt controlData.txt gwrsys.txt 0.3 0.2 0.02 0.06 1.8 -1 0.01 0.01 2.5 2.5 2.5 2.5 2.5 2.5
