{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
--   handler monad.
--
module Application where

------------------------------------------------------------------------------
import Data.Lens.Template
import Data.Time.Clock
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.MongoDB
import Data.Lens.Common

import Prelude hiding ((.))
import Control.Category ((.))

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App),
      _database :: Snaplet MongoDB
    }

makeLens ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasMongoDB App where
  getMongoDB = getL (snapletValue . database)

------------------------------------------------------------------------------
type AppHandler = Handler App App


