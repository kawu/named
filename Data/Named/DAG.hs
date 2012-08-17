module Data.Named.DAG
( DAG
, mkDAG
, NodeI (..)
, Node (..)
, children
, inNodes
, roots
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V

-- | DAG with distinguished types for internal and leaf nodes.
type DAG n l = V.Vector (NodeI Int n l)

data NodeI i n l
    = InnerI n [i]
    | LeafI l
    deriving (Show, Read, Eq, Ord)

-- | TODO: Check, that argument is really a DAG (not a general graph).
mkDAG
    :: Ord i
    => [(i, l)]
    -> [(i, n, [i])]
    -> DAG n l
mkDAG ls ns = V.fromList $
    [InnerI n (map enc is) | (_, n, is) <- ns] ++
    [LeafI l | (_, l) <- ls]
  where
    im = M.fromList (zip is [0..])
    is = map fst ls ++ map fst' ns
    enc i = case M.lookup i im of
        Just x  -> x
        Nothing -> error $ "mkDAG: pointer to nothing"
    fst' (x, _, _) = x

data Node n l
    = Inner n [Node n l]
    | Leaf l
    deriving (Show, Read, Eq, Ord)

children :: Node n l -> [Node n l]
children (Leaf _)      = []
children (Inner _ xs) = xs

-- | Internal (branching) nodes.
inNodes :: DAG n l -> [Node n l]
inNodes dag =
    M.elems nm
  where
    nm = M.fromList
        [ (i, mkNode n)
        | (i, n) <- zip [0..] (V.toList dag) ]
    mkNode (InnerI n is) = Inner n (map dec is)
    mkNode (LeafI l)     = Leaf l
    dec i = case M.lookup i nm of
        Just n  -> n
        Nothing -> error "roots: pointer to nothing"

roots :: (Ord n, Ord l) => DAG n l -> [Node n l]
roots dag =
    let noRoots = S.fromList $ concatMap children $ inNodes dag
    in  filter (not . flip S.member noRoots) (inNodes dag)
