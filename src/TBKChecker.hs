module TBKChecker where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as M
import Data.List
import Control.Monad

import TBAPIServer
import Model

checkTBKForItems :: (TBItem a, Ord a) => Set a -> IO (Set a)
checkTBKForItems itemSet = liftM (updateItems . M.unions) $ mapM getTBKUrlsForItems $ accum $ Set.toList $ Set.map itemId itemSet
  where accum = getFirst . foldl' check40 ([],[],0)
        check40 (l,i,40) b = (i:l, [b], 0)
        check40 (l,i,n) b = (l, b:i, n + 1)
        getFirst (l,_,_) = l
        updateItems urlMap = Set.map (updateTBK urlMap) itemSet
        updateTBK urlMap item = case itemTBKUrl item of
          Just _ -> item
          Nothing -> setTBKUrl item $ M.lookup (itemId item) urlMap
