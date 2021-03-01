{-# LANGUAGE GADTs #-}

module Foread where

import Data.Tuple (swap)
import Control.Applicative ((<|>))
import Control.Monad ((>=>))

import Text.ParserCombinators.ReadP (string,sepBy)

import Util.ReadPMaybe (tryParser,preParse)
import Util.Util (guarded)

import Format
import Equivalences


foread :: Format t -> String -> Maybe t
foread (FAtom i _) = i
foread (FTrans i _ f) = i >=> foread f
foread (F1 ab _ fa) = (ab<$>) . foread fa
foread (F2 abc _ fa _ fb) = tryParser (abc <$> preParse (foread fa)
                                           <*> preParse (foread fb))
foread (F3 abcd _ fa _ fb _ fc) = tryParser (abcd <$> preParse (foread fa)
                                                  <*> preParse (foread fb)
                                                  <*> preParse (foread fc))
foread (F4 abcde _ fa _ fb _ fc _ fd) = tryParser (abcde <$> preParse (foread fa)
                                                         <*> preParse (foread fb)
                                                         <*> preParse (foread fc)
                                                         <*> preParse (foread fd))
foread (FFork f0 f1) = \str -> foread f1 str
                           <|> foread f0 str
foread (FCond b f1 f0) = \str -> (foread f1 str >>= guarded b)
                             <|> (foread f0 str >>= guarded (not.b))
foread (FInJect fet e) = foread fet >=> (\(e',t') -> if e' == e
                                    then Just t' else Nothing)
foread (FDict d) = flip lookup (map swap d)
foread (FList sep f) = tryParser $ preParse (foread f) `sepBy` string sep
foread f = foread $ formeq f
