module Model.Solution where

------------------------------------------------------------------------------
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple


------------------------------------------------------------------------------
-- |
data Solution = Solution
    { solutionId        :: Int
    , solutionStudentId :: Int     -- ^ creator of the solution
    , solutionTaskId    :: Int
    , solutionContent   :: String
    , solutionScore     :: Int
    , solutionTime      :: UTCTime
    } deriving (Show)

instance FromRow Solution where
    fromRow = Solution
                <$> field
                <*> field
                <*> field
                <*> field
                <*> field
                <*> field
