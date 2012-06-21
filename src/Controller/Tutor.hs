{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller to handle the tutor dashboard page.
module Controller.Tutor
    ( handleTutorArea
    ) where

------------------------------------------------------------------------------
import qualified Data.Text as T
import           Snap.Snaplet.Heist
import           Text.Templating.Heist
------------------------------------------------------------------------------
import           Application
import           Lib.Auth
import           Model.Course
import           Model.Task


------------------------------------------------------------------------------
-- | Renders the course list and the task list into the tutors area.
handleTutorArea :: AppHandler ()
handleTutorArea = withTutor $ do
    uid     <- getAuthUserId
    let courses = [] -- FIXME
    --courses <- getTutorCourses uid
    let tasks = [] -- FIXME
    --tasks   <- getTutorTasks   uid
    let splices = [] --tutorSplices courses tasks
    heistLocal (bindSplices splices) $ render "tutor/index"
{-
  where
    tutorSplices courses tasks =
      [ ("courseList", mapSplices renderCourse courses)
      , ("taskList",   mapSplices renderTask   tasks)
      ]

------------------------------------------------------------------------------
-- | Splice to render one course into a HTML list element.
renderCourse :: Course -> Splice AppHandler
renderCourse course =
    runChildrenWithText [ ("courseId",   T.pack . show $ courseId course)
                        , ("courseName", T.pack $ courseName course)
                        ]


------------------------------------------------------------------------------
-- | Splice to render one task into a HTML list element.
renderTask :: Task -> Splice AppHandler
renderTask task =
    runChildrenWithText [ ("taskId",   T.pack . show $ taskId task)
                        , ("taskName", T.pack $ taskName task)
                        ]
-}
