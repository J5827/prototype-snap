{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for creating and managing courses.
module Controller.Course
    ( handleCourseForm
    ) where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T
import           Snap
import           Snap.Snaplet.Heist
import           Text.Digestive.Form
import           Text.Digestive.Snap
import           Text.Templating.Heist
------------------------------------------------------------------------------
import           Application
--import           Model.Course
import           Lib.Auth
import           Lib.Form


------------------------------------------------------------------------------
-- | Render a form to display a course.
handleCourseForm :: AppHandler ()
handleCourseForm = withTutor $ do
    (view, courseData) <- runForm "form" courseForm
    maybe (showForm "course" view) createNewCourse courseData


------------------------------------------------------------------------------
-- | Data required in the course form.
data CourseData = CourseData
  { courseName     :: Text
  , courseCapacity :: Text
  } deriving (Show)


------------------------------------------------------------------------------
-- | Create a new course from the entered data.
--
-- TODO Submit a user information message into the session that can then be
-- displayed on the main page instead of displaying a separate page.
createNewCourse :: CourseData -> AppHandler ()
createNewCourse (CourseData name capacity) = do
    uid <- getAuthUserId
    let success = False -- FIXME create course in DB => WHERE?
    -- success <- createCourse (T.unpack name) (read $ T.unpack capacity) uid
    let template = if success
                     then "messages/course-create-success"
                     else "messages/course-create-failure"
    heistLocal (bindString "courseName" name) $ render template


------------------------------------------------------------------------------
-- | Digestive course form.
courseForm :: Form Text AppHandler CourseData
courseForm = CourseData
      <$> "coursename" .: check coursenameEmptyMsg notEmpty (text Nothing)
      <*> "capacity"   .: text Nothing


------------------------------------------------------------------------------
-- | Error message for missing course name.
coursenameEmptyMsg :: Text
coursenameEmptyMsg = "bitte Kursnamen eingeben"

