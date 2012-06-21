{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Auth helper library to provide functions such as authentication guards or
-- to get the id from the current logged in user.
module Lib.Auth
    ( getAuthUserId
    , unwrapUserId
    , requireTutor
    , requireStudent
    , withTutor
    , withStudent
    ) where

------------------------------------------------------------------------------
import           Data.Maybe
import qualified Data.Text as T
import           Snap
import           Snap.Snaplet.Auth hiding (Role)
------------------------------------------------------------------------------
import           Application
import           Controller.Error
import           Model.User hiding (userId)


------------------------------------------------------------------------------
-- | Unwraps the user id until an Int value from a passed AuthUser.
unwrapUserId :: AuthUser -> Int
unwrapUserId = read . T.unpack . unUid . fromJust . userId


------------------------------------------------------------------------------
-- | Unwraps the userId record field of the currently logged in auth user.
-- Uses the unsafe fromJust, so it need to be called after a user check (e.g.
-- withAuth) has already performed.
getAuthUserId :: AppHandler Int
getAuthUserId = unwrapUserId <$> fromJust <$> with auth currentUser


------------------------------------------------------------------------------
-- | Require tutor for a handler otherwise switch to alternative handler.
requireTutor :: AppHandler () -> AppHandler () -> AppHandler ()
requireTutor = requireRole Tutor


------------------------------------------------------------------------------
-- | Require tutor for a handler otherwise switch to error handler.
withTutor :: AppHandler () -> AppHandler ()
withTutor = flip requireTutor (handleError "permission denied")


------------------------------------------------------------------------------
-- | Require student for a handler otherwise switch to alternative handler.
requireStudent :: AppHandler () -> AppHandler () -> AppHandler ()
requireStudent = requireRole Student


------------------------------------------------------------------------------
-- | Require student for a handler otherwise switch to error handler.
withStudent :: AppHandler () -> AppHandler ()
withStudent = flip requireStudent (handleError "permission denied")


------------------------------------------------------------------------------
-- | Allow only user with a specific role the access to the handler.
--
-- If the user is not authenticated or has the wrong role redirect to the
-- index handler. Allow only user with a specific role the access to the
-- handler.
requireRole :: Role -> AppHandler () -> AppHandler () -> AppHandler ()
requireRole role targetHandler alternativeHandler = do
    authUserId <- getAuthUserId 
    user  <- loadUserByAuthId authUserId
    if userRole user == role
       then targetHandler
       else alternativeHandler
