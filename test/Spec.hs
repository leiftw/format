{-# LANGUAGE DeriveDataTypeable #-}

--module Spec where

import Data.Data
import Data.Maybe (fromJust)

import Formats

--import Formed

import Forite
import Foread


data Tripli = Primo | Alto Bool | Ultimo Int deriving Data

tripliform :: Format Tripli
tripliform = FCond ((=="Primo").showConstr.toConstr) -- avoid hard string?
                   (FConst Primo "ONE")
             (FConst (Alto False) "MORE")

--fo :: Format String
--fo = FAtom Just id
--fo = F2In const id (FAtom Just id) "_" null (FC_ "the empty string")


main :: IO ()
main = putStrLn $ forite tripliform
     $ fromJust $ foread tripliform "MORE"
