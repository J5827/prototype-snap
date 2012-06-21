{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module DataTypes where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import           Data.Typeable
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.Ok

