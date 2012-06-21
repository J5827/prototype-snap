{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Data.Lens.Template
import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Auth
import Snap.Snaplet.Session


------------------------------------------------------------------------------
-- | The application snaplet data type which contains other snaplets as
-- subsnaplet.
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager App)
    , _db    :: Snaplet Postgres
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasPostgres (Handler b App) where
    getPostgresState = with db get

------------------------------------------------------------------------------
type AppHandler = Handler App App
