{-# LANGUAGE GADTs #-}

module Format where

import GHC.Exts (IsList,Item)
import Data.Set (Set)

import qualified Data.Map as M (Map)


data Format a where
-- theoretical
 FEmpty :: Format a -- `F(Bottom|Zero)`?
 FUnit :: String -> Format ()
 FConst :: a -> String -> Format a -- information loss!
-- basic
 FAtom :: (String -> Maybe a) -> (a -> String) -> Format a
 FTrans :: (String -> Maybe String) -> (String -> String) -> Format a -> Format a -- hacky
 FPrefix :: String -> Format a -> Format a -- `PreF`?
 FSuffix :: String -> Format a -> Format a -- `SufF`?
 -- products
 F1 :: (a -> b) -> (b -> a) -> Format a -> Format b
 F2 :: (a -> b -> c) -> (c -> a) -> Format a
                     -> (c -> b) -> Format b -> Format c
 F3 :: (a -> b -> c -> d) -> (d -> a) -> Format a
                          -> (d -> b) -> Format b
                          -> (d -> c) -> Format c -> Format d
 F4 :: (a -> b -> c -> d -> e) -> (e -> a) -> Format a
                               -> (e -> b) -> Format b
                               -> (e -> c) -> Format c
                               -> (e -> d) -> Format d -> Format e
 F2ple :: Format a -> Format b -> Format (a,b)
 F3ple :: Format a -> Format b -> Format c -> Format (a,b,c)
 FWrap :: (a -> b -> c) -> (c -> a) -> (a1 -> a2 -> a) -> (a -> a1) -> Format a1
                        -> (c -> b)                                 -> Format b
                                                       -> (a -> a2) -> Format a2 -> Format c
 FWraple :: (a1 -> a2 -> a) -> (a -> a1) -> Format a1
                                         -> Format b
                            -> (a -> a2) -> Format a2 -> Format (a,b)
 -- arrow-like
 FFst :: (a -> b) -> (b -> a) -> Format (a,c) -> Format (b,c)
 FSnd :: (a -> b) -> (b -> a) -> Format (c,a) -> Format (c,b)
 -- control flow
 FMaybe :: String -> Format a -> Format (Maybe a)
 FEither :: Format l -> Format r -> Format (Either l r) -- rename short?
 FAlt :: Format a -> Format a -> Format a -- completes semiring
 FCond :: (a -> Bool) -> Format a -> Format a -> Format a
 --FWich :: [(a -> Bool),Format a] -> Format a -> Format a -- experimental for `FCond` chains
 FOptn :: String -> Format Bool -- doubled as pattern
 FBool :: String -> String -> Format Bool -- doubled as pattern
 -- equality
 FFork :: (Eq a) => Format a -> Format a -> Format a -- `Eq` unfortunately necessary, restricts semiring
 FInJect :: (Eq e) => Format (e,t) -> e -> Format t
 FXcept :: (Eq e) => e -> String -> Format e -> Format e
 FNtry :: (Eq e) => e -> String -> Format e
 FDict :: (Eq e) => [(e,String)] -> Format e -- default never needed
 FMap :: (Ord e) => M.Map e String -> Format e -- default never needed
 -- faster one way, slower the other (need to `assocs` before `swap`)
                 -- `Set` of tuples?
 -- repetition
 FLdbl :: (Foldable f) => ([a] -> f a) -> (f a -> [a])
                       -> String -> Format a -> Format (f a) -- `-> FormListMode ...`
 FIsList :: (IsList l) => String -> Format (Item l) -> Format l
 FList :: String -> Format a -> Format [a]
 FLSet :: (Ord o) => String -> Format o -> Format (Set o)
 FEnum :: (Foldable f) => ([a] -> f a) -> (f a -> [a]) -> String -> Format a -> Format (f a)
  -- specifically `oxford`, not `list`!

data FormListMode = SimplifyThis
                  { betweentwo :: String
                  , betweenall :: String
                  , beforelast :: String }

-- two possible approaches:
-- an entry point converting the container, and then inner formats for lists per mode
 -- this allows the inner format to take different arguments depending on mode
-- a single format converting the container that takes a mode descriptor
