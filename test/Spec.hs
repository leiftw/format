--module Spec where

import Data.Maybe (fromJust)

{-
import Format
import Patterns
import Utils
import Formed

import Forite
import Foread
-}
import Lib

fo :: Format String
fo = F2In const id (FAtom Just id) "_" null (FC_ "the empty string")


main :: IO ()
main = putStrLn $ forite fo $ fromJust $ foread fo "yo"
