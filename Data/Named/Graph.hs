module Data.Named.Graph
( Graph (..)
, mkGraph
, node
, edges
, roots
, toTree
, toKeyTree
, toForestWith
, toForest
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Tree as T
import Data.List (mapAccumL, foldl', sortBy)
import Data.Maybe (mapMaybe, fromJust)
import Data.Ord (comparing)

data Graph k v = Graph
    { nodeMap :: M.Map k v
    , edgeMap :: M.Map k [k] }

mkGraph :: Ord k => [(k, v, [k])] -> Graph k v
mkGraph xs = 
    Graph ns es
  where
    ns = M.fromList [(k, v)  | (k, v, _)  <- xs]
    es = M.fromList [(k, ks) | (k, _, ks) <- xs]

{-# INLINE node #-}
node :: (Show k, Ord k) => Graph k v -> k -> v
node g k = case M.lookup k (nodeMap g) of
    Nothing -> error $ "node: key " ++ show k ++ " not in the nodes map"
    Just v  -> v

{-# INLINE edges #-}
edges :: (Show k, Ord k) => Graph k v -> k -> [k]
edges g k = case M.lookup k (edgeMap g) of
    Nothing -> error $ "edges: key " ++ show k ++ " not in the edtes map"
    Just v  -> v

roots :: Ord k => Graph k v -> [k]
roots g =
    [ k
    | (k, v) <- M.assocs (nodeMap g)
    , not (k `S.member` desc) ]
  where
    desc = S.fromList . concat . M.elems $ edgeMap g

-- | Make a tree rooted in a given node with respect to a graph.
toTree :: (Show k, Ord k) => Graph k v -> k -> T.Tree v
toTree g = fmap (node g) . toKeyTree g

toKeyTree :: (Show k, Ord k) => Graph k v -> k -> T.Tree k
toKeyTree g k = T.Node k
    [ toKeyTree g k'
    | k' <- edges g k ]

-- | Transform graph into a forest given the priority function.
-- That is, trees with higher priorities will be taken first,
-- while those with lower priorities might be trimmed down
-- (since we don't want to have duplicate nodes in the
-- resulting forest).
toForestWith :: (Show k, Ord k, Ord a)
             => (T.Tree v -> a) -> Graph k v -> T.Forest v
toForestWith pr g = map valTr . snd $
    mapAccumL trim S.empty sortedTrees
  where
    valTr = fmap (node g) -- ^ Make value tree from a key tree
    trees = map (toKeyTree g) (roots g)
    sortedTrees =
        let f = pr . valTr
        in  sortBy (comparing f) trees

-- | Transform graph into a forest. It removes duplicate
-- nodes from trees chosing trees in an arbitrary order.
toForest :: (Show k, Ord k) => Graph k v -> T.Forest v
toForest = toForestWith (const 0)

trim :: Ord k => S.Set k -> T.Tree k -> (S.Set k, T.Tree k)
trim visited tree =
    (visited', tree')
  where
    tree'    = fromJust (doIt tree)
    visited' = foldl' (flip S.insert) visited (T.flatten tree')
    doIt (T.Node x ts)
        | x `S.member` visited = Nothing
        | otherwise = Just $ T.Node x (mapMaybe doIt ts)
