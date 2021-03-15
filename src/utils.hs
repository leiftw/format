module Utils where

import Data.Tuple (swap)
import Data.Maybe (fromJust)
import Control.Arrow ((&&&))
import Numeric.Natural
import Text.Read (readMaybe)

import Util (replicat)

import Format

-- TODO: what decides between `Format` via `formeq`, and `Utils`, and patterns?
-- patterns at least can't contain function application, just constructors


readshowform :: (Show sr,Read sr) => Format sr -- rename!
readshowform = FAtom readMaybe show

 -- write-only without non-zero guarantee
repliform :: Format f -> Format (Natural,f) -- this way around to mirror `replicat(e)`
repliform = FCond ((<1).fst) (FConst (0,undefined) "")
          . F1 ((fromIntegral.length) &&& head) (uncurry replicat) . FLdbl id id ""


formJust :: Format f -> Format (Maybe f)
formJust = F1 Just fromJust

formMaybe :: (t -> Format f) -> Maybe t -> Format (Maybe f)
formMaybe tf Nothing = FConst Nothing "" -- TODO: check `Nothing`, how without `Eq f`?
formMaybe tf (Just x) = formJust $ tf x
--formMaybe tf mt = maybe (FConst Nothing "") (formJust . tf) mt


-- reflexivity! inelegant to need these though

notform :: Format Bool -> Format Bool
notform = F1 not not

nFOptn :: String -> Format Bool -- rename? -- `pattern`?
nFOptn = notform . FOptn

swapform :: Format (a,b) -> Format (b,a)
swapform = F1 swap swap

reverseform :: Format [a] -> Format [a]
reverseform = F1 reverse reverse


-- separate the more concrete stuff?

bracform :: Format t -> Format t
bracform = FPrefix "{"
         . FSuffix "}"

tagform :: Format tt -> Format t -> Format (tt,t)
tagform ft f = FWraple const id (FPrefix "<"  $ FSuffix ">" ft)
                                f
                             id (FPrefix "</" $ FSuffix ">" ft)

tagform_const :: String -> Format t -> Format t -- rename?
tagform_const = flip $ FInJect . tagform (FAtom Just id)
 -- `String` constraint technically unnecessary
--tagform_const :: String -> Format t -> Format t
--tagform_const tag = FPrefix ("<"  ++ tag ++ ">")
--                  . FSuffix ("</" ++ tag ++ ">")
