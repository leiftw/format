{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Patterns where

import Format


pattern FMP s f = FMaybe "" (FPrefix s f)
pattern FMS s f = FMaybe "" (FSuffix s f)

-- write-only!
pattern FC_ s <- FConst _ s
 where FC_ s = FConst undefined s
 --`pattern FC_ s = FConst undefined s` complains that type of `undefined` is not bound
 -- `FAtom undefined (const s)` would hide parse success/failure

pattern F2In abc ca fa s cb fb = F2 abc ca fa cb (FPrefix s fb)
-- confusing names!
pattern FIn fa s fb = F2ple fa (FPrefix s fb)

pattern FO s = FB s ""
-- doubles of actual `Format`s
pattern FB s1 s0 = FDict [(True,s1),(False,s0)]

-- TODO: provide short patterns for common uses

{-
IF :: (a -> Bool) -> Format a -> Format a -> Format a
IF = FCond

FX :: (Eq e) => e -> String -> Format e -> Format e
FX = FXcept

FL :: (Foldable f) => ([a] -> f a) -> String -> Format a -> Format (f a)
FL = FLdbl

FE :: (Foldable f) => ([a] -> f a) -> String -> Format a -> Format (f a)
FE = FEnum
-}

-- TODO: provide combinatorial access to list modes for foldable types, ->.utils?

--pattern FLL flm = FL id id flm
--pattern FLL flm <- FList _ _ flm
-- where FLL flm = FList IL.fromList IL.toList flm
--pattern FLS flm = FList S.fromList toList flm
