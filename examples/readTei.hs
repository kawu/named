import Control.Monad (forM_)
import System.Environment (getArgs)
import qualified Data.Set as S
import qualified Data.Tree as T

import Data.Named.Graph (mkGraph, toTree, roots)
import Text.Named.TeiNCP (readNamed, names, ID, NE (..), Ptr (..), sentences)

mkForest :: [NE] -> T.Forest (Either NE ID)
mkForest ns =
    map (toTree graph) (roots graph)
  where
    graph = mkGraph $
        -- | NE nodes.
        [ ( neID ne
          , Left ne
          , map target (ptrs ne) )
        | ne <- ns ] ++
        -- | Word (segment) nodes.
        [ (ptr, Right ptr, [])
        | ne <- ns
        , Global ptr _ <- ptrs ne ]

main = do
    [teiPath] <- getArgs
    parts <- readNamed teiPath
    forM_ parts $ \(path, para) -> do
        putStrLn $ "### " ++ path
        let sents = concatMap sentences para
            forests = map (mkForest  . names) sents
        forM_ forests $ \forest -> do
            putStrLn $ T.drawForest (map (fmap show) forest)
