module Util.Util where

import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty(..))
import Numeric.Natural

import Control.Applicative (Applicative,Alternative,pure,empty,liftA2)


n_one :: [t] -> NonEmpty (Maybe t)
n_one xs = Nothing :| map Just xs


breaks :: (t -> Bool) -> [t] -> [[t]]
breaks f = unfoldr (Just . break f)
--breaks _ [] = []
--breaks f (x:xs) | f x       = [x] : breaks f xs
--                | otherwise = x :.: breaks f xs
--where
--(:.:) :: t -> [[t]] -> [[t]]
--x :.: [] = [[x]]
--x :.: (xs:xss) = (x:xs):xss


-- clashes with `Generic.Random.(%)`, rename to (<%>) or (-<)!
-- `?` in C
-- §utility-ht-0.0.14/Data-Bool-HT.(?:) and §if-0.1.0.0/If.(?)
-- inside-uncurried `Data.Bool.bool` and internal `ifThenElse`
-- see https://mail.haskell.org/pipermail/libraries/2016-November/thread.html#27426
-- represented by `Format.FBool`
(%) :: Bool -> (t,t) -> t
(%) True (s,_) = s
(%) False (_,s) = s


-- used in ReadPMaybe,OrdUtil,SUtil
-- is also in Set.Util and Dep_Twelf
infixr 8 .|. -- in (f .|. g . h) precedence does not matter
             --    (f . g .|. h) does not make sense
(.|.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d -- rename?
f .|. g = (f .) . g


-- move to *`NatUtil`?

repliconcat :: Natural -> [a] -> [a]
repliconcat = concat .|. replicat

replicat :: Natural -> a -> [a]
replicat = replicate . fromIntegral


-- move, or get from alternative prelude?
guarded :: (Alternative a) => (t -> Bool) -> t -> a t
guarded f x = if f x then pure x else empty

foldApp :: (Foldable f,Applicative a) => (t -> t -> t) -> f (a t) -> a t
foldApp b = foldr1 (liftA2 b)
