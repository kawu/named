{-# LANGUAGE DeriveFunctor #-}

module Data.Named
( Named (..)
, mapWords
, Ne (..)
, Child
, mapWords'
, names
, mkNamed
) where

import Prelude hiding (words)
import qualified Data.Text as T
import qualified Data.Vector as V

import Data.Named.DAG hiding (children)
import qualified Data.Named.DAG as D

-- | Structure of named entities is a DAG.
data Named w n = Named
    -- | We represent NE nodes and leaf nodes separately.
    { dag   :: DAG n Int
    -- | Leaf nodes indicate particular words in a words vector.
    , words :: [w] }
    deriving (Show)

mapWords :: (w -> w') -> Named w n -> Named w' n
mapWords f (Named dag ws) = Named dag (map f ws)

data Ne w t = Ne
    { orth      :: T.Text
    , label     :: t
    , children  :: [Child w t] }
    deriving (Show, Read, Eq, Ord, Functor)

-- | Each NE child is either word with additional position information
-- or a named entity.
type Child w t = Either (Int, w) (Ne w t)

mapWords' :: (w -> w') -> Ne w t -> Ne w' t
mapWords' f ne = ne { children = map (mapWordCh f) (children ne) }
mapWordCh f (Left (k, w)) = Left (k, f w)
mapWordCh f (Right ne)    = Right (mapWords' f ne)

names :: Ord t => Named w (T.Text, t) -> [Ne w t]
names nmd =
    map mkNe (roots (dag nmd))
  where
    mkNe (Inner (orth, tag) xs) = Ne
        { orth = orth
        , label = tag
        , children = map mkChild xs }
    mkNe (Leaf _) = error "names: leaf named entity"
    mkChild      (Leaf k)    = Left (k, wordAt k)
    mkChild node@(Inner _ _) = Right (mkNe node)
    wordAt k
        | k < 0 || k >= V.length wordV  =
            error "names: word index out of bounds"
        | otherwise = wordV V.! k
    wordV = V.fromList (words nmd)

mkNamed
    :: Ord i
    => [(i, w)]         -- ^ Words with identifiers
    -> [(i, n, [i])]    -- ^ NEs with identifiers and pointer lists
    -> Named w n
mkNamed ws ns = 
    -- | Substitute indices for words.
    let wsIxs = [(i, k) | (k, (i, _w)) <- zip [0..] ws]
    in  Named (mkDAG wsIxs ns) (map snd ws)
