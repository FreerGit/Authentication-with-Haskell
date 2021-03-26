{-# LANGUAGE OverloadedStrings #-}

module Authentication.JWT where

import qualified Data.Map as Map
import           Data.Text
import           Web.JWT
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX
import           Data.Aeson.Types (Value (Bool, Number))
import           Prelude hiding (exp)
import           Data.Int (Int64)

import           Config

type Token = Text

mkJWT :: UTCTime -> JWTsecret -> Int64 -> Token
mkJWT currentTime secret uid = 
    let cs =
            mempty -- mempty returns a default JWTClaimsSet
            { iss = stringOrURI "AuthieAtYourService"
            , unregisteredClaims =
                ClaimsMap $
                    Map.fromList [("USERID", Number $ fromIntegral uid)],
                exp =
                    numericDate $
                        utcTimeToPOSIXSeconds currentTime + 30 * posixDayLength
            }
        signer = hmacSecret secret
        in encodeSigned signer mempty cs

-- verifyJWT :: UTCTime -> SecretKey -> Token -> Maybe Int
-- verifyJWT currentTime secret token = do
--   let signer = hmacSecret secret
--   unverifiedJWT <- decode token
--   verifiedJWT <- verify signer unverifiedJWT
--   expTime <- exp . claims $ verifiedJWT
--   now <- numericDate $ utcTimeToPOSIXSeconds currentTime
--   guard (now < expTime)
--   let kv = unClaimsMap . unregisteredClaims . claims $ verifiedJWT
--   userIDVal <- Map.lookup userIDKey kv
--   case userIDVal of
--     Number userID -> return . fromIntegral $ coefficient userID
--     _ -> Nothing