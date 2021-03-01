module Formaring where

import Semiring

import Format


-- picking a `Semigroup`/`Monoid` out of these would be arbitrary
instance Semiring (Format a) where
 zero = FEmpty
 plus = FAlt
 -- this addition seems like `Alternative`, but not even `Functor` (see README)
 one = FConst undefined "" -- `Patterns.FC_ ""`, import?
 times = FFork
 -- `F2ple` is just as good multiplication, but goes across types
