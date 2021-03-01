{-# LANGUAGE TupleSections #-}

module Util.ReadPMaybe where -- interface for `ReadP` with `Nothing` as failure

import Data.Maybe (listToMaybe,mapMaybe)
import Data.Foldable (toList)

import Text.ParserCombinators.ReadP (ReadP,readP_to_S,readS_to_P)

import Data.Tuple.Extra (firstM)

import Util.Util ((.|.))
import Util.StrUtil (splits)


tryParser :: ReadP a -> String -> Maybe a
tryParser = fmap fst . listToMaybe .|. readP_to_S

preParse :: (String -> Maybe a) -> ReadP a
preParse f = readS_to_P (mapMaybe (firstM f) . splits)

uniParse :: (String -> Maybe a) -> ReadP a
uniParse f = readS_to_P (map (,"") . toList . f)
