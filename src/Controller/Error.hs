{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller to handle error messages.
--
module Controller.Error
    ( handleError
    ) where

------------------------------------------------------------------------------
import           Data.Text (Text)
import           Snap
import           Snap.Snaplet.Heist
import           Text.Templating.Heist
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- |
handleError :: Text -> AppHandler ()
handleError errorMsg =
    heistLocal (bindString "errorMsg" errorMsg) $ render "error"
