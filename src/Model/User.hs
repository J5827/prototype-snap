{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

------------------------------------------------------------------------------
-- |
module Model.User where

------------------------------------------------------------------------------
import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import           Data.Typeable
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.Ok
import           Snap.Snaplet.PostgresqlSimple
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- |
data User = User
    { userId         :: Int
    , userAuthUserId :: Int
    , userFirstname  :: String
    , userLastname   :: String
    , userEmail      :: String
    , userSchool     :: School
    , userRole       :: Role
    } deriving (Show)

instance FromRow User where
    fromRow = User
                <$> field
                <*> field
                <*> field
                <*> field
                <*> field
                <*> field
                <*> field


loadUserByAuthId :: Int -> AppHandler User
loadUserByAuthId aid = do
    rows <- query "select * from users where auth_user_id = ?" (Only aid)
    return $ head rows

createUser :: (Int, Text, Text, Text, School, Role) -> AppHandler ()
createUser values = do
    _ <- execute queryString values
    return ()
  where
    queryString :: Query
    queryString =
      "insert into users (auth_user_id, firstname, lastname, email, school, role) values (?,?,?,?,?,?)"


------------------------------------------------------------------------------
-- |
data School = HtwkLeipzig
            | UniLeipzig
            | UniHalle
            | UniKassel
            | FhZwickau
            | FsuJena
            | PhHeidelberg
            deriving (Eq, Read, Show, Typeable)

instance FromField School where
    fromField _ (Just s) = Ok . read . BS.unpack $ s
    fromField f Nothing  = returnError ConversionFailed f "school is null"

instance ToField School where
    toField = Escape . BS.pack . show

------------------------------------------------------------------------------
-- | Sets a list of tuples with names corresponding to each school.
schools :: [(School, Text)]
schools =
  [ (HtwkLeipzig,  "HTWK Leipzig")
  , (UniLeipzig,   "Uni Leipzig")
  , (UniHalle,     "Uni Halle")
  , (UniKassel,    "Uni Kassel")
  , (FhZwickau,    "FH Zwickau")
  , (FsuJena,      "FSU Jena")
  , (PhHeidelberg, "PH Heidelberg")
  ]


------------------------------------------------------------------------------
-- |
data Role = Student
          | Tutor
          deriving (Eq, Read, Show, Typeable)


instance FromField Role where
    fromField _ (Just s) = Ok . read . BS.unpack $ s
    fromField f Nothing  = returnError ConversionFailed f "role is null"

instance ToField Role where
    toField = Escape . BS.pack . show

------------------------------------------------------------------------------
-- | Sets a list of tuples with names corresponding to each role.
role :: [(Role, Text)]
role =
    [ (Student, "Student")
    , (Tutor,   "Referent")
    ]
