{-# LANGUAGE FlexibleInstances #-} -- Show,Read => Formed, Formed String
{-# LANGUAGE UndecidableInstances #-} -- Show,Read => Formed

module Formed where

import Data.Foldable (toList)
import Data.MultiSet (MultiSet,fromList)
import Text.Read (readMaybe)

import Format


-- TODO: different formats/versions/languages
-- on the type level: `Formed l f`
-- on the value level: `form :: l -> Format f`
-- name that `Translate`? or this `UniFormed`?
class Formed f where -- rename `Form(atable)`?
 form :: Format f


instance Formed String where
 form = FAtom Just id

instance {-# OVERLAPPABLE #-} (Show sr,Read sr) => Formed sr where
 form = FAtom readMaybe show -- `Utils.readshowform`, import?

-- TODO: default recur `form` (also using /grammar/(conscase|spechars).txt)
-- `instance {-# OVERLAPS #-} Generic g => Formed g`?


instance Formed f => Formed (Maybe f) where
 form = FMaybe "" $ FPrefix " " form -- `Patterns.FMP " " form`, import?

instance (Formed fl,Formed fr) => Formed (Either fl fr) where
 form = FEither form form

instance (Ord f,Formed f) => Formed (MultiSet f) where
 form = FLdbl fromList toList "" form
