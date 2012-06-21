module Model.Enrollment where

------------------------------------------------------------------------------
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple


------------------------------------------------------------------------------
-- |
data Enrollment = Enrollment
    { enrollmentStudentId :: Int
    , enrollmentGroupId   :: Int
    , enrollmentTime      :: UTCTime
    }

instance FromRow Enrollment where
    fromRow = Enrollment
                <$> field
                <*> field
                <*> field
