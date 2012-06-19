module DataModel where

import Data.Time.Clock (UTCTime)

------------------------------------------------------------------------------
-- PERSISTENT DATA TYPES                                                    --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- |
data Tutor = Tutor
    { tutorId        :: Int
    , tutorFirstname :: String
    , tutorLastname  :: String
    , tutorSchool    :: School
    }
    

------------------------------------------------------------------------------
-- |
data Student = Student
    { studentId        :: Int
    , studentFirstname :: String
    , studentLastname  :: String
    , studentSchool    :: School
    }


------------------------------------------------------------------------------
-- |
data Course = Course
    { courseId         :: Int
    , courseTutor      :: Tutor
    , courseName       :: String
    , courseSemester   :: Semester
    , courseAssessment :: Maybe (Int, Int)          -- ^ (required, total)
    , courseEnrollment :: Maybe (UTCTime, UTCTime)  -- ^ (start, deadline)
    }


------------------------------------------------------------------------------
-- |
data Group = Group
    { groupId       :: Int
    , groupCourse   :: Course
    , groupTutor    :: Tutor
    , groupName     :: String
    , groupCapacity :: Int
    }


------------------------------------------------------------------------------
-- |
data Task = Task
    { taskId      :: Int
    , taskTutor   :: Tutor    -- ^ creator of the task
    , taskName    :: String   -- ^ customized task name
    , taskStatus  :: Status
    , taskScoring :: Scoring
    , taskConfig  :: Config
    }


------------------------------------------------------------------------------
-- |
data Solution = Solution
    { solutionId      :: Int
    , solutionStudent :: Int     -- ^ creator of the solution
    , solutionTask    :: Task
    , solutionContent :: String
    , solutionScore   :: Int
    }


------------------------------------------------------------------------------
-- |
data Assignment = Assignment
    { assignmentTask      :: Task
    , assignmentCourse    :: Course
    , assignmentStartTime :: UTCTime
    , assignmentDeadline  :: UTCTime
    }


------------------------------------------------------------------------------
-- |
data Enrollment = Enrollment
    { enrollmentStudent :: Student
    , enrollmentGroup   :: Group
    , enrollmentTime    :: UTCTime
    }


------------------------------------------------------------------------------
-- COMPLEX DATA TYPES                                                       --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- |
data Config = Config
    { configTag  :: String  -- ^ name of task on server
    , configBody :: String  -- ^ server formatted task configuration
    , configKey  :: String  -- ^ hash key (actual signature)
    } deriving (Show)


------------------------------------------------------------------------------
-- ATTRIBUTE DATA TYPES                                                     --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- |
data Semester = WS11
              | SS12

instance Show Semester where
    show WS11 = "WS11/12 (vorbei)"
    show SS12 = "SS12"


------------------------------------------------------------------------------
-- |
data Status = Mandatory
            | Facultative

instance Show Status where
   show Mandatory   = "obligatorisch"
   show Facultative = "fakultativ"


------------------------------------------------------------------------------
-- |
data School = HtwkLeipzig
            | UniLeipzig
            | UniHalle
            | UniKassel
            | WOG
            | FhZwickau
            | FsuJena
            | PhHeidelberg

instance Show School where
    show HtwkLeipzig  = "HTWK Leipzig"
    show UniLeipzig   = "Uni Leipzig"
    show UniHalle     = "Uni Halle"
    show UniKassel    = "Uni Kassel"
    show FhZwickau    = "FH Zwickau"
    show FsuJena      = "FSU Jena"
    show PhHeidelberg = "PH Heidelberg"


------------------------------------------------------------------------------
-- |
data Scoring = Increasing
             | Decreasing
             | None

instance Show Scoring where
    show Increasing = "aufsteigend"
    show Decreasing = "absteigend"
    show None       = "keine"
