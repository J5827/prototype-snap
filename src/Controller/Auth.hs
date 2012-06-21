{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller to handle user identification and authentication via a login
-- form.
--
module Controller.Auth
    ( handleLoginForm
    , handleLogout
    , handleUserIdentification
    ) where

------------------------------------------------------------------------------
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Text.Digestive.Form
import           Text.Digestive.Snap
------------------------------------------------------------------------------
import           Application
import           Controller.Error
import           Lib.Auth
import           Lib.Form
import           Model.User


------------------------------------------------------------------------------
-- | Forward anonymous user to the login page and authenticated user to a role
-- specific page.
handleUserIdentification :: AppHandler ()
handleUserIdentification = ifTop
    $ requireUser auth (render "landing")
    $ requireTutor   (redirect "tutor")
    $ requireStudent (redirect "student")
    $ handleError "unsupported user role"


------------------------------------------------------------------------------
-- | Displays the login form. (later)
handleLoginForm :: AppHandler ()
handleLoginForm = do
    (view, loginData) <- runForm "form" loginForm
    maybe (showForm "login" view) performLogin loginData


------------------------------------------------------------------------------
-- | Handler to perform the logout action.
handleLogout :: AppHandler ()
handleLogout = do
    with auth logout
    redirect "/"


------------------------------------------------------------------------------
-- | Data required in the login form.
data LoginData = LoginData
  { loginSchool   :: School
  , loginUsername :: Text
  , loginPassword :: Text
  , loginRemember :: Bool
  } deriving (Show)


------------------------------------------------------------------------------
-- | Set the user authenticated to the session after he the login data has
-- alreay been approved by the login form.
performLogin :: LoginData -> AppHandler ()
performLogin loginData = do
    with auth . loginByUsername username password $ loginRemember loginData
    redirect "/"
  where
    username = T.encodeUtf8 $ loginUsername loginData
    password = ClearText . T.encodeUtf8 $ loginPassword loginData


------------------------------------------------------------------------------
-- | Login form for a user.
loginForm :: Form Text AppHandler LoginData
loginForm =
    checkM invalidLoginMsg verifyLogin $ LoginData
      <$> "school"   .: choice schools Nothing
      <*> "username" .: check usernameEmptyMsg notEmpty (text Nothing)
      <*> "password" .: check passwordEmptyMsg notEmpty (text Nothing)
      <*> "remember" .: bool (Just False)


------------------------------------------------------------------------------
-- | Try to find the user in the user backend store. Then check, if the role
-- is correct and if the password hash matches. Returns True (in the Handler
-- monad) if every test has passed correctly.
verifyLogin :: LoginData -> AppHandler Bool
verifyLogin loginData = do
    authMgr  <- with auth get
    authUser <- liftIO . lookupByLogin authMgr $ loginUsername loginData
    return $ maybe False authenticate authUser
  where
    authenticate = isNothing . flip authenticatePassword password
    password = ClearText . T.encodeUtf8 $ loginPassword loginData


------------------------------------------------------------------------------
-- | Error message for missing username.
usernameEmptyMsg :: Text
usernameEmptyMsg = "bitte Matrikelnummer eingeben"


------------------------------------------------------------------------------
-- | Error message for missing password.
passwordEmptyMsg :: Text
passwordEmptyMsg = "bitte Passwort eingeben"


------------------------------------------------------------------------------
-- | Error message for invalid login.
invalidLoginMsg :: Text
invalidLoginMsg  = "falsches Passwort"
