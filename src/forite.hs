{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Forite where

import Data.Maybe (fromJust)
import Control.Arrow ((&&&))

import qualified Data.Map as M (lookup)

import StrUtil (intermap)

import Format
import Equivalences


forite :: Format t -> t -> String
-- theoretical
forite FEmpty = const undefined
-- basic
forite (FAtom _ o) = o
forite (FTrans _ o f) = o . forite f
-- products
forite (F1 _ ba fa) = forite fa . ba
forite (F2 _ ca fa cb fb) = uncurry(++) . (forite fa . ca
                                       &&& forite fb . cb)
forite (F3 _ da fa db fb dc fc) = uncurry(++)
                                .(uncurry(++) . (forite fa . da
                                             &&& forite fb . db)
                                &&& forite fc . dc)
-- = forite (F2 undefined (da &&& db) (forite (F2ple fa fb))
--                        dc fc)
-- recur to `F2` case? rather as a `formeq`?
-- = forite (F2 undefined (ea &&& eb) (forite (F2ple fa fb))
--                        (ec &&& ed) (forite (F2ple fc fd)))
forite (F4 _ ea fa eb fb ec fc ed fd) = uncurry(++)
                                      . ((uncurry(++) . (forite fa . ea
                                                     &&& forite fb . eb))
                                      &&&(uncurry(++) . (forite fc . ec
                                                     &&& forite fd . ed)))
-- control flow
forite (FAlt f0 f1) = forite f0 -- TODO: how to detect `undefined` from `FEmpty` and other failures?
forite (FCond b f1 f0) = \x -> if b x then forite f1 x else forite f0 x
-- equality
forite (FFork f0 f1) = uncurry(++) . (forite f0
                                  &&& forite f1)
forite (FInJect fet e) = forite fet . (e,)
-- repetition
forite (FDict d) = fromJust . flip lookup d -- intentionally partial
forite (FMap d) = fromJust . flip M.lookup d -- intentionally partial
forite (FList sep f) = intermap (forite f) sep
-- equivalence
forite f = forite $ formeq f
