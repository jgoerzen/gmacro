{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

module Utils where

import Data.List

combine [] _ = []
combine ((k,v):xs) other =
    case lookup k other of
         Just res -> (v, res) : combine xs other
         Nothing -> combine xs other

