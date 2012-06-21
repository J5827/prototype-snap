{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Student controller to handle student functionalities.
module Controller.Student
    ( handleStudentArea
    ) where

------------------------------------------------------------------------------
import qualified Data.Text as T
import           Snap.Snaplet.Heist
import           Text.Templating.Heist
------------------------------------------------------------------------------
import           Application
import           Lib.Auth
import           Model.Course
--import           Model.Student
import           Model.Task


------------------------------------------------------------------------------
-- | Renders the student area.
handleStudentArea :: AppHandler ()
handleStudentArea = withStudent $ do
    uid          <- getAuthUserId
    let enrolCourse = [] -- FIXME load all courses for enrollment from DB
    -- enrolCourses <- getAvailableCourses uid
    let myCourses = [] -- FIXME load courses of student from DB
    -- myCourses    <- getStudentCourses   uid
    let splices = [] --studentSplices enrolCourses myCourses
    heistLocal (bindSplices splices) $ render "student/index"
{-
  where
    studentSplices enrolCourses myCourses =
      [ ("enrolCourseList", mapSplices renderEnrolCourse    enrolCourses)
      , ("myCourseList",    mapSplices renderCourseWithTask myCourses)
      ]


------------------------------------------------------------------------------
-- | Splice to render a course to enrol into a HTML list element.
renderEnrolCourse :: Course -> Splice AppHandler
renderEnrolCourse course =
    runChildrenWithText
      [ ("enrolCourseId",   T.pack . show $ courseId course)
      , ("enrolCourseName", T.pack $ courseName course)
      ]


------------------------------------------------------------------------------
-- | Splice to render a course with all its tasks into a HTML list element.
renderCourseWithTask :: Course -> Splice AppHandler
renderCourseWithTask course =
    runChildrenWith
      [ ("myCourseName",  textSplice . T.pack $ courseName course)
      , ("myCourseTasks", mapSplices renderTask $ courseTasks course)
      ]

------------------------------------------------------------------------------
-- | Splice to render a task into a HTML list element.
renderTask :: Task -> Splice AppHandler
renderTask task =
    runChildrenWithText
      [ ("taskId",   T.pack . show $ taskId task)
      , ("taskName", T.pack $ taskName task)
      ]
-}
