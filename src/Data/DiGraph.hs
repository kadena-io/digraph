{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: DiGraph
-- Copyright: Copyright © 2018-2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Directed graphs in adjacency set representation. The implementation is based
-- on "Data.HashMap.Strict" and "Data.HashSet" from the [unordered-containers
-- package](https://hackage.haskell.org/package/unordered-containers)
--
-- Undirected graphs are represented as symmetric, irreflexive directed graphs.
--
module Data.DiGraph
( DiGraph
, DiEdge
, adjacencySets
, vertices
, edges
, adjacents
, incidents

-- * Construction and Modification of Graphs
, insertEdge
, fromEdges
, insertVertex
, mapVertices
, union
, transpose
, symmetric
, fromList
, unsafeFromList

-- * Predicates
, isDiGraph
, isAdjacent
, isRegular
, isSymmetric
, isIrreflexive
, isEdge
, isVertex

-- * Properties
, order
, size
, diSize
, symSize
, outDegree
, inDegree
, maxOutDegree
, maxInDegree
, minOutDegree
, minInDegree

-- * Distances, Shortest Paths, and Diameter
, ShortestPathCache
, shortestPathCache
, shortestPath
, shortestPath_
, distance
, distance_
, diameter
, diameter_

-- * Graphs
, emptyGraph
, singleton
, clique
, pair
, triangle
, cycle
, diCycle
, line
, diLine
, petersonGraph
, twentyChainGraph
, hoffmanSingleton

) where

import Control.Arrow
import Control.DeepSeq
import Control.Monad

import Data.Foldable
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import Data.Maybe
import Data.Semigroup
import Data.Traversable
import Data.Tuple

import GHC.Generics

import Numeric.Natural

import Prelude hiding (cycle)

-- internal modules

import qualified Data.DiGraph.FloydWarshall as FW

-- -------------------------------------------------------------------------- --
-- Utils

int :: Integral a => Num b => a -> b
int = fromIntegral
{-# INLINE int #-}

-- -------------------------------------------------------------------------- --
-- Graph

-- | Directed Edge.
--
type DiEdge a = (a, a)

-- | Adjacency set representation of directed graphs.
--
-- It is assumed that each target of an edge is also explicitly a vertex in the
-- graph.
--
-- It is not generally required that graphs are irreflexive, but all concrete
-- graphs that are defined in this module are irreflexive.
--
-- Undirected graphs are represented as symmetric directed graphs.
--
newtype DiGraph a = DiGraph { unGraph :: HM.HashMap a (HS.HashSet a) }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData, Hashable)

instance (Hashable a, Eq a) => Semigroup (DiGraph a) where
    (DiGraph a) <> (DiGraph b) = DiGraph (HM.unionWith (<>) a b)
    {-# INLINE (<>) #-}

instance (Hashable a, Eq a) => Monoid (DiGraph a) where
    mempty = DiGraph mempty
    mappend = (<>)
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

-- | A predicate that asserts that every target of an edge is also a vertex in
-- the graph. Any graph that is constructed without using unsafe methods is
-- guaranteed to satisfy this predicate.
--
isDiGraph :: Eq a => Hashable a => DiGraph a -> Bool
isDiGraph g@(DiGraph m) = HS.null (HS.unions (HM.elems m) `HS.difference` vertices g)
{-# INLINE isDiGraph #-}

-- | The adjacency sets of a graph.
--
adjacencySets :: DiGraph a -> HM.HashMap a (HS.HashSet a)
adjacencySets = unGraph
{-# INLINE adjacencySets #-}

-- | The set of vertices of the graph.
--
vertices :: DiGraph a -> HS.HashSet a
vertices = HS.fromMap . HM.map (const ()) . unGraph
{-# INLINE vertices #-}

-- | The set edges of the graph.
--
edges :: Eq a => Hashable a => DiGraph a -> HS.HashSet (DiEdge a)
edges = HS.fromList . concatMap (traverse HS.toList) . HM.toList . unGraph
{-# INLINE edges #-}

-- | The set of adjacent pairs of a graph.
--
adjacents :: Eq a => Hashable a => a -> DiGraph a -> HS.HashSet a
adjacents a (DiGraph g) = g HM.! a
{-# INLINE adjacents #-}

-- | The set of incident edges of a graph.
--
incidents :: Eq a => Hashable a => a -> DiGraph a -> [(a, a)]
incidents a g = [ (a, b) | b <- toList (adjacents a g) ]
{-# INLINE incidents #-}

-- -------------------------------------------------------------------------- --
-- Constructing and Modifying Graphs

-- | Construct a graph from adjacency lists.
--
fromList :: Eq a => Hashable a => [(a,[a])] -> DiGraph a
fromList l = foldr insertVertex es (fst <$> l)
  where
    es = fromEdges [ (a,b) | (a, bs) <- l, b <- bs ]
{-# INLINE fromList #-}

-- | Unsafely construct a graph from adjacency lists.
--
-- This function assumes that the input includes a adjacency list of each vertex
-- that appears in a adjacency list of another vertex. Generally, 'fromList'
-- should be preferred.
--
unsafeFromList :: Eq a => Hashable a => [(a,[a])] -> DiGraph a
unsafeFromList = DiGraph . HM.map HS.fromList . HM.fromList
{-# INLINE unsafeFromList #-}

-- | Construct a graph from a foldable structure of edges.
--
fromEdges :: Eq a => Hashable a => Foldable f => f (a, a) -> DiGraph a
fromEdges = foldr insertEdge mempty
{-# INLINE fromEdges #-}

-- | The union of two graphs.
--
union :: Eq a => Hashable a => DiGraph a -> DiGraph a -> DiGraph a
union = (<>)
{-# INLINE union #-}

-- | Map a function over all vertices of a graph.
--
mapVertices :: Eq b => Hashable b => (a -> b) -> DiGraph a -> DiGraph b
mapVertices f = DiGraph . HM.fromList . fmap (f *** HS.map f) . HM.toList . unGraph
{-# INLINE mapVertices #-}

-- | Transpose a graph, i.e. reverse all edges of the graph.
--
transpose :: Eq a => Hashable a => DiGraph a -> DiGraph a
transpose g = (DiGraph $ mempty <$ unGraph g)
    `union` (fromEdges . HS.map swap $ edges g)

-- | Symmetric closure of a graph.
--
symmetric :: Eq a => Hashable a => DiGraph a -> DiGraph a
symmetric g = g <> transpose g
{-# INLINE symmetric #-}

-- | Insert an edge. Returns the graph unmodified if the edge is already in the
-- graph. Non-existing vertices are added.
--
insertEdge :: Eq a => Hashable a => DiEdge a -> DiGraph a -> DiGraph a
insertEdge (a,b) = DiGraph
    . HM.insertWith (<>) a [b]
    . HM.insertWith (<>) b []
    . unGraph
{-# INLINE insertEdge #-}

-- | Insert a vertex. Returns the graph unmodified if the vertex is already in
-- the graph.
--
insertVertex :: Eq a => Hashable a => a -> DiGraph a -> DiGraph a
insertVertex a = DiGraph . HM.insertWith (<>) a [] . unGraph
{-# INLINE insertVertex #-}

-- -------------------------------------------------------------------------- --
-- Properties

-- | The order of a graph is the number of vertices.
--
order :: DiGraph a -> Natural
order = int . HS.size . vertices
{-# INLINE order #-}

-- | Directed Size. This the number of edges of the graph.
--
diSize :: Eq a => Hashable a => DiGraph a -> Natural
diSize = int . HS.size . edges
{-# INLINE diSize #-}

-- | Directed Size. This the number of edges of the graph.
--
size :: Eq a => Hashable a => DiGraph a -> Natural
size = int . HS.size . edges
{-# INLINE size #-}

-- | Undirected Size of a graph. This is the number of edges of the symmetric
-- closure of the graph.
--
symSize :: Eq a => Hashable a => DiGraph a -> Natural
symSize g = diSize (symmetric g) `div` 2
{-# INLINE symSize #-}

-- | The number of outgoing edges of vertex in a graph.
--
outDegree :: Eq a => Hashable a => DiGraph a -> a -> Natural
outDegree (DiGraph g) a = int . HS.size $ g HM.! a
{-# INLINE outDegree #-}

-- | The maximum out-degree of the vertices of a graph.
--
maxOutDegree :: Eq a => Hashable a => DiGraph a -> Natural
maxOutDegree g = maximum $ HS.map (outDegree g) (vertices g)
{-# INLINE maxOutDegree #-}

-- | The minimum out-degree of the vertices of a graph.
--
minOutDegree :: Eq a => Hashable a => DiGraph a -> Natural
minOutDegree g = minimum $ HS.map (outDegree g) (vertices g)
{-# INLINE minOutDegree #-}

-- | The number of incoming edges of vertex in a graph.
--
inDegree :: Eq a => Hashable a => DiGraph a -> a -> Natural
inDegree g = outDegree (transpose g)
{-# INLINE inDegree #-}

-- | The maximum in-degree of the vertices of a graph.
--
maxInDegree :: Eq a => Hashable a => DiGraph a -> Natural
maxInDegree = maxOutDegree . transpose
{-# INLINE maxInDegree #-}

-- | The minimum in-degree of the vertices of a graph.
--
minInDegree :: Eq a => Hashable a => DiGraph a -> Natural
minInDegree = minOutDegree . transpose
{-# INLINE minInDegree #-}

-- -------------------------------------------------------------------------- --
-- Predicates

-- | Return whether a graph is regular, i.e. whether all vertices have the same
-- out-degree. Note that the latter implies that all vertices also have the same
-- in-degree.
--
isRegular :: DiGraph a -> Bool
isRegular = (== 1)
    . length
    . L.group
    . fmap (HS.size . snd)
    . HM.toList
    . unGraph
{-# INLINE isRegular #-}

-- | Return whether a graph is symmetric, i.e. whether for each edge \((a,b)\)
-- there is also the edge \((b,a)\) in the graph.
--
isSymmetric :: Hashable a => Eq a => DiGraph a -> Bool
isSymmetric g = all checkVertex $ HM.toList $ unGraph g
  where
    checkVertex (a, e) = all (\x -> isAdjacent x a g) e
{-# INLINE isSymmetric #-}

-- | Return whether a graph is irreflexive. A graph is irreflexive if for each
-- edge \((a,b)\) it holds that \(a \neq b\), i.e there are no self-loops in the
-- graph.
--
isIrreflexive :: Eq a => Hashable a => DiGraph a -> Bool
isIrreflexive = not . any (uncurry HS.member) . HM.toList . unGraph
{-# INLINE isIrreflexive #-}

-- | Return whether a vertex is contained in a graph.
--
isVertex :: Eq a => Hashable a => a -> DiGraph a -> Bool
isVertex a = HM.member a . unGraph
{-# INLINE isVertex #-}

-- | Return whether an edge is contained in a graph.
--
isEdge :: Eq a => Hashable a => DiEdge a -> DiGraph a -> Bool
isEdge (a, b) = maybe False (HS.member b) . HM.lookup a . unGraph
{-# INLINE isEdge #-}

-- | Return whether two vertices are adjacent in a graph.
--
isAdjacent :: Eq a => Hashable a => a -> a -> DiGraph a -> Bool
isAdjacent = curry isEdge
{-# INLINE isAdjacent #-}

-- -------------------------------------------------------------------------- --
-- Distances, Shortest Paths, and Diameter

-- | The shortest path matrix of a graph.
--
-- The shortest path matrix of a graph can be used to efficiently query the
-- distance and shortest path between any two vertices of the graph. It can also
-- be used to efficiently compute the diameter of the graph.
--
-- Computing the shortest path matrix is expensive for larger graphs. The matrix
-- is computed using the Floyd-Warshall algorithm. The space and time complexity
-- is quadratic in the /order/ of the graph. For sparse graphs there are more
-- efficient algorithms for computing distances and shortest paths between the
-- nodes of the graph.
--
data ShortestPathCache a = ShortestPathCache
    { _spcMatrix :: {-# UNPACK #-} !FW.ShortestPathMatrix
        -- ^ The shortest path matrix of a graph.
    , _spcIndices :: !(HM.HashMap a Int)
        -- ^ mapping from vertices of the graph to indices in the shortest path
        -- matrix.
    , _spcVertices :: !(HM.HashMap Int a)
        -- ^ mapping from indices in the shortest path matrix to vertices in the
        -- graph.
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

-- | Compute the shortest path matrix for a graph. The result can be used to
-- efficiently query the distance and shortest path between any two vertices of
-- the graph. It can also be used to efficiently compute the diameter of the
-- graph.
--
shortestPathCache :: Eq a => Hashable a => DiGraph a -> ShortestPathCache a
shortestPathCache g = ShortestPathCache m vmap rvmap
  where
    m = FW.floydWarshall $ FW.fromAdjacencySets (unGraph ig)
    ig = mapVertices (vmap HM.!) g
    vmap = HM.fromList $ zip (HS.toList $ vertices g) [0..]
    rvmap = HM.fromList $ zip [0..] (HS.toList $ vertices g)

-- | Compute the Diameter of a graph, i.e. the maximum length of a shortest path
-- between two vertices in the graph.
--
-- This is expensive to compute for larger graphs. If also the shortest paths or
-- distances are needed, one should use 'shortestPathCache' to cache the result
-- of the search and use the 'diameter_', 'shortestPath_', and 'distance_' to
-- query the respective results from the cache.
--
-- The algorithm is optimized for dense graphs. For large sparse graphs a more
-- efficient algorithm should be used.
--
diameter :: Eq a => Hashable a => DiGraph a -> Maybe Natural
diameter = diameter_ . shortestPathCache
{-# INLINE diameter #-}

-- | Compute the Diameter of a graph from a shortest path matrix. The diameter
-- of a graph is the maximum length of a shortest path between two vertices in
-- the graph.
--
diameter_ :: ShortestPathCache a -> Maybe Natural
diameter_ (ShortestPathCache m _ _) = round <$> FW.diameter m
{-# INLINE diameter_ #-}

-- | Compute the shortest path between two vertices of a graph.
--
-- | This is expensive for larger graphs. If more than one path is needed one
-- should use 'shortestPathCache' to cache the result of the search and use
-- 'shortestPath_' to query paths from the cache.
--
-- The algorithm is optimized for dense graphs. For large sparse graphs a more
-- efficient algorithm should be used.
--
shortestPath :: Eq a => Hashable a => a -> a -> DiGraph a -> Maybe [a]
shortestPath src trg = shortestPath_ src trg . shortestPathCache
{-# INLINE shortestPath #-}

-- | Compute the shortest path between two vertices from the shortest path
-- matrix of a graph.
--
-- The algorithm is optimized for dense graphs. For large sparse graphs a more
-- efficient algorithm should be used.
--
shortestPath_ :: Eq a => Hashable a => a -> a -> ShortestPathCache a -> Maybe [a]
shortestPath_ src trg (ShortestPathCache c m r)
    = fmap ((HM.!) r) <$> FW.shortestPath c (m HM.! src) (m HM.! trg)
{-# INLINE shortestPath_ #-}

-- | Compute the distance between two vertices of a graph.
--
-- | This is expensive for larger graphs. If more than one distance is needed
-- one should use 'shortestPathCache' to cache the result of the search and use
-- 'distance_' to query paths from the cache.
--
-- The algorithm is optimized for dense graphs. For large sparse graphs a more
-- efficient algorithm should be used.
--
distance :: Eq a => Hashable a => a -> a -> DiGraph a -> Maybe Natural
distance src trg = distance_ src trg . shortestPathCache
{-# INLINE distance #-}

-- | Compute the distance between two vertices from the shortest path matrix of
-- a graph.
--
-- The algorithm is optimized for dense graphs. For large sparse graphs a more
-- efficient algorithm should be used.
--
distance_ :: Eq a => Hashable a => a -> a -> ShortestPathCache a -> Maybe Natural
distance_ src trg (ShortestPathCache c m _)
    = round <$> FW.distance c (m HM.! src) (m HM.! trg)
{-# INLINE distance_ #-}

-- -------------------------------------------------------------------------- --
-- Concrete Graph

-- | The empty graph on @n@ nodes. This is the graph of 'order' @n@ and 'size'
-- @0@.
--
emptyGraph :: Natural -> DiGraph Int
emptyGraph n = unsafeFromList [ (i, []) | i <- [0 .. int n - 1] ]

-- | Undirected clique.
--
clique :: Natural -> DiGraph Int
clique i = unsafeFromList
    [ (a, b)
    | a <- [0 .. int i - 1]
    , let b = [ x | x <- [0 .. int i - 1] , x /= a ]
    ]

-- | The (irreflexive) singleton graph.
--
singleton :: DiGraph Int
singleton = clique 1

-- | Undirected pair.
--
pair :: DiGraph Int
pair = clique 2

-- | Undirected triangle.
--
triangle :: DiGraph Int
triangle = clique 3

-- | Directed cycle.
--
diCycle :: Natural -> DiGraph Int
diCycle n = unsafeFromList [ (a, [(a + 1) `mod` int n]) | a <- [0 .. int n - 1] ]

-- | Undirected cycle.
--
cycle :: Natural -> DiGraph Int
cycle = symmetric . diCycle

-- | Directed line.
--
diLine :: Natural -> DiGraph Int
diLine n = unsafeFromList [ (a, [ a + 1 | a /= int n - 1]) | a <- [0 .. int n - 1] ]

-- | Undirected line.
--
line :: Natural -> DiGraph Int
line = symmetric . diLine

-- | The Peterson graph.
--
petersonGraph :: DiGraph Int
petersonGraph = DiGraph
    [ (0, [2,3,5])
    , (1, [3,4,6])
    , (2, [4,0,7])
    , (3, [0,1,8])
    , (4, [1,2,9])
    , (5, [0,6,9])
    , (6, [1,5,7])
    , (7, [2,6,8])
    , (8, [3,7,9])
    , (9, [4,8,5])
    ]

-- | The "twenty chain" graph.
--
twentyChainGraph :: DiGraph Int
twentyChainGraph  = pentagram `union` pentagon1 `union` pentagon2 `union` connections
  where
    pentagram = mapVertices (+ 5) $ pentagon2pentagram $ cycle 5
    pentagon1 = mapVertices (+ 10) $ cycle 5
    pentagon2 = mapVertices (+ 15) $ cycle 5
    connections = fromEdges $ HS.fromList $ mconcat
        [ [(i, x), (x, i)]
        | i <- [0..4]
        , x <- [i + 5, i + 10, i + 15]
        ]
    pentagon2pentagram = mapVertices $ \case
        0 -> 0
        1 -> 3
        2 -> 1
        3 -> 4
        4 -> 2
        _ -> error "invalid vertex"

-- | Hoffman-Singleton Graph.
--
-- The Hoffman-Singleton graph is a 7-regular graph with 50 vertices and 175
-- edges. It's the largest graph of max-degree 7 and diameter 2. Cf.
-- [https://en.wikipedia.org/wiki/Hoffman–Singleton_graph]()
--
hoffmanSingleton :: DiGraph Int
hoffmanSingleton = pentagons `union` pentagrams `union` connections
  where
    pentagons = mconcat
        [ mapVertices (p_off i) $ cycle 5 | i <- [0 .. 4] ]
    pentagrams = mconcat
        [ mapVertices (q_off i) $ pentagon2pentagram $ cycle 5 | i <- [0 .. 4] ]

    p_off h = (+) (25 + 5 * h)
    q_off i = (+) (5 * i)

    pentagon2pentagram = mapVertices $ \case
        0 -> 0
        1 -> 3
        2 -> 1
        3 -> 4
        4 -> 2
        _ -> error "invalid vertex"

    connections = fromEdges $ HS.fromList $ mconcat
        [ [(a, b), (b, a)]
        | h <- [0 .. 4]
        , j <- [0 .. 4]
        , let a = p_off h j
        , i <- [0 .. 4]
        , let b = q_off i ((h * i + j) `mod` 5)
        ]
