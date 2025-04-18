{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module: Data.DiGraph.FloydWarshall
-- Copyright: Copyright © 2018-2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Data.DiGraph.FloydWarshall
(
-- * Graph Representations
  DenseAdjMatrix
, AdjacencySets

-- * Conversions
, fromAdjacencySets
, toAdjacencySets

-- * FloydWarshall Algorithm
, ShortestPathMatrix(..)
, floydWarshall
, shortestPath
, distance
, diameter

-- * Legacy exports
, distMatrix_
, floydWarshall_
, diameter_
, shortestPaths_
) where

import Control.DeepSeq

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable
#endif
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Massiv.Array as M

import GHC.Generics

import Numeric.Natural

-- -------------------------------------------------------------------------- --
-- Graph Representations

-- | Adjacency matrix representation of a directed graph.
--
type DenseAdjMatrix = Array U Ix2 Int

-- | Adjacency set representation of a directed graph.
--
type AdjacencySets = HM.HashMap Int (HS.HashSet Int)

-- -------------------------------------------------------------------------- --
-- Adjacency Matrix for dense Graphs
--
-- (uses massiv)

-- | Assumes that the input is an directed graph and that the vertex
-- set is a prefix of the natural numbers.
--
fromAdjacencySets :: AdjacencySets -> DenseAdjMatrix
fromAdjacencySets g = makeArray Seq (Sz (n :. n)) go
  where
    n = HM.size g
    go (i :. j)
        | isEdge (i, j) = 1
        | otherwise = 0
    isEdge (a, b) = maybe False (HS.member b) $ HM.lookup a g

-- | Converts an adjacency matrix into a graph in adjacnency set representation.
--
toAdjacencySets :: DenseAdjMatrix -> AdjacencySets
toAdjacencySets = ifoldlS f mempty
  where
    f a (i :. j) x
        | x == 0 = a
        | otherwise = HM.insertWith (<>) i (HS.singleton j) a

-- -------------------------------------------------------------------------- --
-- Floyd Warshall with Paths

-- | Shortest path matrix of a graph.
--
newtype ShortestPathMatrix = ShortestPathMatrix (Array U Ix2 (Double, Int))
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (NFData)

-- | Shortest path computation for integral matrixes.
--
floydWarshall :: Unbox a => Real a => Array U Ix2 a -> ShortestPathMatrix
floydWarshall = ShortestPathMatrix
    . floydWarshallInternal
    . computeAs U
    . intDistMatrix

-- | Compute a shortest path between two vertices of a graph from the shortest
-- path matrix of the graph.
--
shortestPath
    :: ShortestPathMatrix
    -> Int
    -> Int
    -> Maybe [Int]
shortestPath (ShortestPathMatrix m) src trg
    | M.isEmpty mat = Nothing
    | not (M.isSafeIndex (size m) (src :. trg)) = Nothing
    | (mat M.! (src :. trg)) == (-1) = Nothing
    | otherwise = go src trg
  where
    mat = M.computeAs U $ M.map snd m
    go a b
        | a == b = return []
        | otherwise = do
            n <- M.index mat (a :. b)
            (:) n <$> go n b

-- | Compute the distance between two vertices of a graph from the shortest path
-- matrix of the graph.
--
distance :: ShortestPathMatrix -> Int -> Int -> Maybe Double
distance (ShortestPathMatrix m) src trg
    | M.isEmpty m = Nothing
    | otherwise = toDistance . fst =<< M.index m (src :. trg)

-- | Compute the diameter of a graph from the shortest path matrix of the graph.
--
diameter :: ShortestPathMatrix -> Maybe Double
diameter (ShortestPathMatrix m)
    | M.isEmpty m = Just 0
    | otherwise = toDistance $ maximum' $ M.map fst m

-- -------------------------------------------------------------------------- --
-- Internal

toDistance :: RealFrac a => a -> Maybe a
toDistance x
    | x == 1/0 = Nothing
    | otherwise = Just x

-- | Distance matrix for int inputs.
--
intDistMatrix
    :: Real a
#if MIN_VERSION_massiv(1,0,0)
    => Source r a
#else
    => Source r Ix2 a
#endif
    => Array r Ix2 a
    -> Array M.D Ix2 (Double, Int)
intDistMatrix = M.imap go
  where
    go (x :. y) e
        | x == y = (0, y)
        | e > 0 = (realToFrac e, y)
        | otherwise = (1/0, -1)

-- | Floyd-Warshall With Path Matrix
--
-- TODO: use a mutable array?
-- TODO: implement Dijkstra's algorithm for adj matrix representation.
--
floydWarshallInternal
    :: Array U Ix2 (Double, Int)
    -> Array U Ix2 (Double,Int)
floydWarshallInternal a = foldl' go a [0..n-1]
  where
    Sz (n :. _) = size a

    go :: Array U Ix2 (Double, Int) -> Int -> Array U Ix2 (Double,Int)
    go c k = makeArray Seq (Sz (n :. n)) $ \(x :. y) ->
        let
            !xy = fst $! c M.! (x :. y)
            !xk = fst $! c M.! (x :. k)
            !ky = fst $! c M.! (k :. y)
            !nxy = snd $! c M.! (x :. y)
            !nxk = snd $! c M.! (x :. k)
        in if xy > xk + ky then (xk + ky, nxk) else (xy, nxy)

-- -------------------------------------------------------------------------- --
-- Floyd Warshall Without Paths (more efficient, by factor of 2)

-- | Floyd Warshall Without Paths (more efficient, by factor of 2).
--
distMatrix_
#if MIN_VERSION_massiv(1,0,0)
    :: Source r Int
#else
    :: Source r Ix2 Int
#endif
    => Array r Ix2 Int
    -> Array M.D Ix2 Double
distMatrix_ = M.imap go
  where
    go (x :. y) e
        | x == y = 0
        | e > 0 = realToFrac e
        | otherwise = 1/0

-- | Floyd Warshall Without Paths (more efficient, by factor of 2).
--
-- TODO: use a mutable array?
-- TODO: implement Dijkstra's algorithm for adj matrix representation.
--
floydWarshall_
    :: Array U Ix2 Double
    -> Array U Ix2 Double
floydWarshall_ a = foldl' go a [0..n-1]
  where
    Sz (n :. _) = size a

    go :: Array U Ix2 Double -> Int -> Array U Ix2 Double
    go c k = makeArray Seq (Sz (n :. n)) $ \(x :. y) ->
        let
            !xy = c M.! (x :. y)
            !xk = c M.! (x :. k)
            !ky = c M.! (k :. y)
        in if xy > xk + ky then xk + ky else xy

-- | Shortest path matrix.
--
-- All entries of the result matrix are either whole numbers or @Infinity@.
--
shortestPaths_ :: Array U Ix2 Int -> Array U Ix2 Double
shortestPaths_ = floydWarshall_ . computeAs U . distMatrix_

-- | Diameter of a graph.
--
diameter_ :: Array U Ix2 Int -> Maybe Natural
diameter_ g
    | M.isEmpty g = Just 0
    | otherwise = let x = round $ maximum' $ shortestPaths_ g
        in if x == round (1/0 :: Double) then Nothing else Just x
