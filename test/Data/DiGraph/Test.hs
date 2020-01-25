{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Data.DiGraph.Test
-- Copyright: Copyright © 2019 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Data.DiGraph.Test
(
-- * Test Properties
  properties
) where

import Data.Bifunctor
import Data.Hashable
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

import Numeric.Natural

import Test.QuickCheck

-- internal modules

import Data.DiGraph

-- -------------------------------------------------------------------------- --
-- Test Properties

prefixProperties :: String -> [(String, Property)] -> [(String, Property)]
prefixProperties p = fmap $ first (p <>)

properties_undirected :: Eq a => Hashable a => DiGraph a -> [(String, Property)]
properties_undirected g =
    [ ("isDiGraph", property $ isDiGraph g)
    , ("isIrreflexive", property $ isIrreflexive g)
    , ("isSymmetric", property $ isSymmetric g)
    ]

properties_emptyGraph :: Natural -> [(String, Property)]
properties_emptyGraph n = prefixProperties ("emptyGraph of order " <> show n <> ": ")
    $ ("order == " <> show n, order g === n)
    : ("size == 0", size g === 0)
    : properties_undirected g
  where
    g = emptyGraph n

properties_singletonGraph :: [(String, Property)]
properties_singletonGraph = prefixProperties "singletonGraph: "
    $ ("order == 1", order g === 1)
    : ("size == 0", symSize g === 0)
    : ("outDegree == 0", maxOutDegree g === 0)
    : ("isRegular", property $ isRegular g)
    : ("diameter == 0", diameter g === Just 0)
    : properties_undirected g
  where
    g = singleton

properties_petersonGraph :: [(String, Property)]
properties_petersonGraph = prefixProperties "petersonGraph: "
    $ ("order == 10", order g === 10)
    : ("size == 15", symSize g === 15)
    : ("outDegree == 3", maxOutDegree g === 3)
    : ("isRegular", property $ isRegular g)
    : ("diameter == 2", diameter g === Just 2)
    : properties_undirected g
  where
    g = petersonGraph

properties_twentyChainGraph :: [(String, Property)]
properties_twentyChainGraph = prefixProperties "twentyChainGraph: "
    $ ("order == 20", order g === 20)
    : ("size == 30", symSize g === 30)
    : ("outDegree == 3", maxOutDegree g === 3)
    : ("isRegular", property $ isRegular g)
    : ("diameter == 2", diameter g === Just 4)
    : properties_undirected g
  where
    g = twentyChainGraph

properties_hoffmanSingletonGraph :: [(String, Property)]
properties_hoffmanSingletonGraph = prefixProperties "HoffmanSingletonGraph: "
    $ ("order == 50", order g === 50)
    : ("size == 175", symSize g === 175)
    : ("outDegree == 7", maxOutDegree g === 7)
    : ("isRegular", property $ isRegular g)
    : ("diameter == 2", diameter g === Just 2)
    : properties_undirected g
  where
    g = hoffmanSingleton

-- | Test Properties.
--
properties :: [(String, Property)]
properties = (concat :: [[(String, Property)]] -> [(String, Property)])
    [ properties_emptyGraph 0
    , properties_emptyGraph 2
    , properties_singletonGraph
    , properties_petersonGraph
    , properties_twentyChainGraph
    , properties_hoffmanSingletonGraph
    ]
