module Util.StrUtil where

import Data.List (isPrefixOf,inits,tails) --stripPrefix,
import Data.Char (isSpace)
import Data.Maybe (fromJust)


-- rename?
(\\\) :: (Eq e) => e -> e -> (e -> e)
(\\\) re by it = if it == re then by else it

replaceholder :: (Eq e) => e -> e -> Bool -> [e] -> [e]
replaceholder c s True  = map (c \\\ s)
replaceholder c s False = filter (/= c)

subreplace :: String -> String -> String -> String
subreplace this by ""  = "" -- so `head` is safe
subreplace this by in_ = if this `isPrefixOf` in_
                         then by      ++ subreplace this by (drop (length this) in_)
                         else head in_ : subreplace this by (tail               in_)
                         --maybe (  head in_ : subreplace this by (tail               in_))
                         --      (((by++).subreplace this by) <$> (this `stripPrefix` in_))


-- represented by `Format.FLdbl`
intermap :: (Foldable f) => (t -> [c]) -> [c] -> f t -> [c]
intermap f i xs | null xs   = [] -- so `drop` is safe
                | otherwise = drop (length i) $ concatMap ((i++).f) xs
--intermap f i = drop (length i) $ foldr ((++).(++i).f) ""
--intermap f i = foldr1 ((++).(++i)) . foldr ((:).f) []
--intermap f i = intercalate i . map f . toList

-- used by `ReadPUtil`
splits :: String -> [(String,String)]
splits s = zip (inits s) (tails s) -- `zip` safely symmetric

-- not quite language-specific (enough for `Grammar`)
tag :: String -> String -> String
tag t c = "<" ++ t ++ ">" ++ c
      ++ "</" ++ t ++ ">"
