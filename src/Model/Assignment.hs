module Model.Assignment where

------------------------------------------------------------------------------
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple


------------------------------------------------------------------------------
-- |
data Assignment = Assignment
    { assignmentTaskId    :: Int
    , assignmentCourseId  :: Int
    , assignmentStartTime :: UTCTime
    , assignmentDeadline  :: UTCTime
    }


instance FromRow Assignment where
    fromRow = Assignment
                <$> field
                <*> field
                <*> field
                <*> field
