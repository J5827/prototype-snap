------------------------------------------------------------------------------
-- | Form helper library containing several functions for verification or form
-- creation.
module Lib.Form
    ( showForm
    , notEmpty
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import qualified Data.Text as T
import           Snap.Snaplet.Heist
import           Text.Digestive.Heist
import           Text.Digestive.View
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Bind the elements from the digestive form to the corresponding view
-- template.
showForm :: String -> View Text -> AppHandler ()
showForm name view =
    heistLocal (bindDigestiveSplices view) $ render template
  where
    template = BS.pack $ "forms/" ++ name


------------------------------------------------------------------------------
-- | Check whether a text has the length zero or not.
notEmpty :: Text -> Bool
notEmpty = not . T.null
