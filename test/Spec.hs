--module Spec where

import Data.Maybe (fromJust)

import Formats

--import Formed

import Forite
import Foread


fo :: Format String
fo = FAtom Just id --F2In const id (FAtom Just id) "_" null (FC_ "the empty string")


main :: IO ()
main = putStrLn $ forite fo $ fromJust $ foread fo "yo"
