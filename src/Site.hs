{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import Data.ByteString (ByteString)
import Snap
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
import Snap.Snaplet.Heist
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe
------------------------------------------------------------------------------
import Application
import Controller.Auth
import Controller.Course
import Controller.Student
import Controller.Task
import Controller.Tutor
import Controller.UserRegistration


------------------------------------------------------------------------------
-- | Test function to play with the database.

{-
handleDb :: AppHandler ()
handleDb = do
    rows <- query_ "select * from solution" -- where id = 1"
    forM_ rows printSolution
  where  
    printSolution :: Solution -> AppHandler ()
    printSolution = writeText . T.pack . show
-}


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/",              handleUserIdentification)
         , ("/course/create", handleCourseForm)
         , ("/login",         handleLoginForm)
         , ("/logout",        handleLogout)
         , ("/register",      handleRegistrationForm)
         , ("/student",       handleStudentArea)
         , ("/tutor",         handleTutorArea)
      -- , ("db",             handleDb)
         , ("",               serveDirectory "resources")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    addRoutes routes
    addAuthSplices auth
    return $ App h s a d
