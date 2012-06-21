{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller to handle user registration via a registration form.
module Controller.UserRegistration
    ( handleRegistrationForm
    ) where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap
import           Snap.Snaplet.Auth hiding (createUser, userId)
import qualified Snap.Snaplet.Auth as A
import           Snap.Snaplet.Heist
import           Text.Digestive.Form
import           Text.Digestive.Snap
import qualified Text.Email.Validate as E
import           Text.Templating.Heist
------------------------------------------------------------------------------
import           Application
import           Lib.Auth
import           Lib.Form
import           Lib.PasswordGenerator
import           Model.User


------------------------------------------------------------------------------
-- | Handler to display and process the registration form.
handleRegistrationForm :: AppHandler ()
handleRegistrationForm = do
    (view, registrationData) <- runForm "form" registrationForm
    maybe (showForm "registration" view) performRegistration registrationData


------------------------------------------------------------------------------
-- | Data required in the login form.
data RegistrationData = RegistrationData
  { regSchool  :: School
  , regUsername  :: Text
  , regFirstname :: Text
  , regLastname  :: Text
  , regEmail     :: Text
  } deriving (Show)


------------------------------------------------------------------------------
-- | Create a new user from the entered registration data. Therefore two
-- entries are made in the database, one in the snap_auth_users table
-- (containing authentication related data) and one in the users table
-- (containing additional user related data). By default every new registered
-- user get the student role. Tutors must currently changed 'by hand', later
-- then possibly by an admin user.
performRegistration :: RegistrationData -> AppHandler ()
performRegistration regData = do
    password <- liftIO createRandomPassword
    authUser <- with auth $ A.createUser uid password
    createUser $ userData (unwrapUserId authUser) regData
    heistLocal (bindStrings $ stringValues password) $
      render "registration-complete"
  where
    uid = regUsername regData
    userData aid (RegistrationData s _ f l e) = (aid, f, l, e, s, Student)
    stringValues password = [
        ("name",     regFirstname regData)
      , ("password", T.decodeUtf8 password)
      ]


------------------------------------------------------------------------------
-- | Login form for a user.
registrationForm :: Form Text AppHandler RegistrationData
registrationForm =
    checkM usernameInUseMsg verifyUsername $ RegistrationData
      <$> "school"    .: choice schools Nothing
      <*> "username"  .: check usernameEmptyMsg  notEmpty (text Nothing)
      <*> "firstname" .: check firstnameEmptyMsg notEmpty (text Nothing)
      <*> "lastname"  .: check lastnameEmptyMsg  notEmpty (text Nothing)
      <*> "email"     .: check emailInvalidMsg verifyEmail (text Nothing)


------------------------------------------------------------------------------
-- | Checks with the auth backend if the username is already in use.
verifyUsername :: RegistrationData -> AppHandler Bool
verifyUsername = liftM not . with auth . usernameExists . regUsername


-----------------------------------------------------------------------------
-- | Function to verify the email via Text.Email.Validate.
--
-- TODO: check if email is already in use
verifyEmail :: Text -> Bool
verifyEmail = E.isValid . T.unpack


-----------------------------------------------------------------------------
-- | Error message for already used username.
usernameInUseMsg :: Text
usernameInUseMsg  = "Matrikelnummer bereits registriert"


-----------------------------------------------------------------------------
-- | Error message for missing username.
usernameEmptyMsg :: Text
usernameEmptyMsg = "bitte Matrikelnummer eingeben"


-----------------------------------------------------------------------------
-- | Error message for missing first name.
firstnameEmptyMsg :: Text
firstnameEmptyMsg = "bitte Vornamen eingeben"


-----------------------------------------------------------------------------
-- | Error message for missing last name.
lastnameEmptyMsg :: Text
lastnameEmptyMsg = "bitte Nachnamen eingeben"


-----------------------------------------------------------------------------
-- | Error message for invalid or missing email.
emailInvalidMsg :: Text
emailInvalidMsg = "keine gültige E-Mail-Adresse"
