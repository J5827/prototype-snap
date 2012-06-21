{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

------------------------------------------------------------------------------
-- | 
module Model.Task where

------------------------------------------------------------------------------
import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import           Data.Typeable
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Ok


------------------------------------------------------------------------------
-- |
data Task = Task
    { taskId      :: Int
    , taskTutorId :: Int      -- ^ creator of the task
    , taskName    :: String   -- ^ customized task name
    , taskStatus  :: Status
    , taskScoring :: Scoring
    , taskConfig  :: Config
    } deriving (Show)

instance FromRow Task where
    fromRow = Task
                <$> field
                <*> field
                <*> field
                <*> field
                <*> field
                <*> field


------------------------------------------------------------------------------
-- |
data Status = Mandatory
            | Facultative
            deriving (Eq, Read, Show, Typeable)


instance FromField Status where
    fromField _ (Just s) = Ok . read . BS.unpack $ s
    fromField f Nothing  = returnError ConversionFailed f "status is null"


------------------------------------------------------------------------------
-- | Sets a list of tuples with names corresponding to each status.
status :: [(Status, Text)]
status =
  [ (Mandatory, "obligatorisch")
  , (Facultative, "fakultativ")
  ]


------------------------------------------------------------------------------
-- |
data Scoring = Increasing
             | Decreasing
             | None
             deriving (Eq, Read, Show, Typeable)


instance FromField Scoring where
    fromField _ (Just s) = Ok . read . BS.unpack $ s
    fromField f Nothing  = returnError ConversionFailed f "scoring is null"


------------------------------------------------------------------------------
-- | Sets a list of tuples with names corresponding to each scoring.
scorings :: [(Scoring, Text)]
scorings =
    [ (Increasing, "aufsteigend")
    , (Decreasing, "absteigend")
    , (None, "keine")
    ]


------------------------------------------------------------------------------
-- |
data Config = Config
    { configTag  :: String  -- ^ name of task on server
    , configBody :: String  -- ^ server formatted task configuration
    , configKey  :: String  -- ^ hash key (actual signature)
    } deriving (Read, Show, Typeable)


instance FromField Config where
    fromField _ (Just s) = Ok . read . BS.unpack $ s
    fromField f Nothing  = returnError ConversionFailed f "config is null"
