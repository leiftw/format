module Operators where

import Format
import Patterns


(++§) :: String -> Format t -> Format t
(++§) = FPrefix

(§++) :: String -> Format t -> Format t
(§++) = FSuffix

(+?§) :: String -> Format t -> Format (Maybe t)
(+?§) = FMP

(§?+) :: String -> Format t -> Format (Maybe t)
(§?+) = FMS
