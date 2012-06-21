module Model.Group where

------------------------------------------------------------------------------
import Database.PostgreSQL.Simple


------------------------------------------------------------------------------
-- |
data Group = Group
    { groupId       :: Int
    , groupCourseId :: Int
    , groupTutorId  :: Int
    , groupName     :: String
    , groupCapacity :: Int
    }

instance FromRow Group where
    fromRow = Group
                <$> field
                <*> field
                <*> field
                <*> field
                <*> field
