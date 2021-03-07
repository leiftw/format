{-# LANGUAGE TypeFamilies #-}

module FO where

import Format


-- can't interleave `forite,foread` definitions
-- https://mail.haskell.org/pipermail/haskell-cafe/2014-November/117116.html

data RMAT = RITE | READ
type family FO rmat t
--type instance FO RITE t = t -> String
--type instance FO READ t = String -> Maybe t
forite = fo RITE
foread = fo READ
fo :: RMAT -> Format t -> FO RMAT t
fo = undefined

-- doesn't work due to `RMAT` crossing from value-level to type-level!
