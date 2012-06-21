------------------------------------------------------------------------------
-- | Module to create a random pasword of alphanumeric chars.
module Lib.PasswordGenerator
  ( createRandomPassword
  ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           System.Random


------------------------------------------------------------------------------
-- | Create a random password.
createRandomPassword :: IO ByteString
createRandomPassword = do
    gen <- newStdGen
    let pw = take 10 $ randomRs ('a', 'z') gen
    return $ BS.pack pw
