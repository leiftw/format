{-# LANGUAGE GADTs #-}

module Equivalences where

import Data.Foldable (toList)
import Data.Set (Set,fromList)
--import Data.List (uncons)
import Data.List (stripPrefix)
import Data.Maybe (fromJust,isNothing)
import Data.Either (isLeft,fromLeft,fromRight)
import Control.Arrow ((&&&),first,second)

import Data.List.Extra (stripSuffix)
import Data.Tuple.Extra (fst3,snd3,thd3)

import Format


formeq :: Format a -> Format a
formeq FEmpty = undefined -- TODO: `FDict []` requires `Eq`...
formeq (FUnit s) = FNtry () s -- could be pattern
formeq (FConst x s) = FAtom (\p -> if p==s then Just x else Nothing) (const s)
                          --(((const x)<$>).guarded(==s))
                          --((>>Just x).guard.(==s))
formeq (FPrefix pre f) = FTrans (stripPrefix pre) (pre++) f
formeq (FSuffix suf f) = FTrans (stripSuffix suf) (++suf) f
formeq (FNtry x s) = FDict [(x,s)] -- could be pattern
formeq (FMaybe s f) = FCond isNothing (FConst Nothing s)
                                     (F1 Just fromJust f) -- `Utils.formJust`, import?
formeq (FEither fl fr) = FCond isLeft (F1 Left  (fromLeft  undefined) fl)
                    -- unsafe `Data.Either.Extra.from*'`?
                                      (F1 Right (fromRight undefined) fr)
formeq (F2ple fa fb) = F2 (,) fst fa snd fb
formeq (F3ple fa fb fc) = F3 (,,) fst3 fa snd3 fb thd3 fc
formeq (FWraple       a1a2a aa1 fa1     fb aa2 fa2) =
        FWrap (,) fst a1a2a aa1 fa1 snd fb aa2 fa2
formeq (FWrap abc ca a1a2a aa1 fa1 cb fb aa2 fa2) = 
        F3 (\a1 b a2 -> abc (a1a2a a1 a2) b)
        -- (flip $ (.|. a1a2a) . flip abc)
        -- ((\a2a -> flip $ abc . a2a) . a1a2a) -- more elegant?
           (aa1.ca) fa1 cb fb (aa2.ca) fa2
formeq (FFst ab ba fac) = F1 (first  ab) (first  ba) fac
formeq (FSnd ab ba fca) = F1 (second ab) (second ba) fca
formeq (FOptn s) = FBool s ""
formeq (FBool s1 s0) = FDict [(True,s1),(False,s0)]
--formeq (FInJect fab b) = F1Maybe (\(a',b') -> if b' == b then Just a' else Nothing)
--                                 (,b) fab
formeq (FXcept x s f) = FCond (==x) (FNtry x s) f
formeq (FLdbl col lis sep f) = F1 col lis $ FList sep f
formeq (FLSet sep f) = FLdbl fromList toList sep f -- TODO: `IsList` stuff?
-- TODO: use `Sord` to `forite` all `Foldable`s except `[]`s
 -- TODO: this could directly operate on lists only to be simpler
formeq (FEnum c l sep f) = FCond null (FConst (c []) "")
                     -- `$ FCond ((==1).length) (F1 (c.(:[])) (head.l) f)`
                         $ FCond ((`elem`[1,2]).length) (FLdbl c l (" "++sep++" ") f)

                         $ F1 (c.uncurry((.(:[])).(++))) ((init &&& last).l)
                         $ F2ple (FLdbl id id ", " f)
                                 (FPrefix (", "++sep++" ") f)
{-
-- `reverse`ing the enumeration: (assumes it's a [])
                       $ F1 (reverse.zip(True:repeat False).reverse) (map snd)
                       $ FLdbl c ", " (F2ple (FOptn (sep++" ")) f)
-- `reverse`ing the `String`s:
                       $ FTrans reverse reverse
                       $ F1 (c.reverse.uncurry(:)) (fromJust.uncons.reverse.l)
                       $ F2ple (FSuffix (" "++sep++" ,") f) (FLdbl id " ," f)
-}
--formeq (FWich bfs f_) = foldr (uncurry FCond) f_ bfs
formeq f = error "format constructor not covered by `forite/foread` nor `formeq`"
