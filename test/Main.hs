{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Main
( main
) where

import Data.Bitraversable
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

import System.Exit

import Test.QuickCheck

-- internal modules

import qualified Data.DiGraph (properties)
import qualified Data.DiGraph.Random (properties)

-- -------------------------------------------------------------------------- --
-- Support for QuickCheck < 2.12

#if ! MIN_VERSION_QuickCheck(2,12,0)
isSuccess :: Result -> Bool
isSuccess Success{} = True
isSuccess _ = False
#endif

-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = do
    results <- traverse (bitraverse print quickCheckResult) properties
    if and $ isSuccess . snd <$> results
        then exitSuccess
        else exitFailure

properties :: [(String, Property)]
properties = mconcat
    [ prefix "Data.DiGraph" <$> Data.DiGraph.properties
    , prefix "Data.DiGraph.Random" <$> Data.DiGraph.Random.properties
    ]
  where
    prefix a (b, c) = (a <> "/" <> b, c)
