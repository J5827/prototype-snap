{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Course where

------------------------------------------------------------------------------
import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Typeable
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Ok


------------------------------------------------------------------------------
-- |
data Course = Course
    { courseId         :: Int
    , courseTutorId    :: Int
    , courseName       :: String
    , courseSemester   :: Semester
    , courseAssessment :: Maybe (Int, Int)          -- ^ (required, total)
    , courseEnrollment :: Maybe (UTCTime, UTCTime)  -- ^ (start, deadline)
    }

instance FromRow Course where
    fromRow = Course
                <$> field
                <*> field
                <*> field
                <*> field
                <*> field
                <*> field

instance FromField (Int, Int) where
    fromField _ (Just s) = Ok . read . BS.unpack $ s
    fromField f Nothing  = returnError ConversionFailed f "assessment is null"

instance FromField (UTCTime, UTCTime) where
    fromField _ (Just s) = Ok . read . BS.unpack $ s
    fromField f Nothing  = returnError ConversionFailed f "enrollment is null"


------------------------------------------------------------------------------
-- |
data Semester = WS11
              | SS12
              deriving (Eq, Read, Show, Typeable)

instance FromField Semester where
    fromField _ (Just s) = Ok . read . BS.unpack $ s
    fromField f Nothing  = returnError ConversionFailed f "semester is null"

------------------------------------------------------------------------------
-- | Sets a list of tuples with names corresponding to each semester.
semesters :: [(Semester, Text)]
semesters =
    [ (WS11, "WS11/12 (vorbei)")
    , (SS12, "SS12")
    ]

