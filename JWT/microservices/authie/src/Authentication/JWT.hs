{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Authentication.JWT where

import qualified Data.Map as Map
import           Data.Text
import           Web.JWT
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX
import           Data.Aeson (ToJSON)
import           Data.Aeson.Types (Value(Number))
import           Prelude hiding (exp)
import           Data.Int (Int64)
import qualified Data.ByteString.Lazy.Internal as BS
import           Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Encoding         as T
import           GHC.Generics

import           Config

import Control.Monad.Trans.Except (throwE, runExceptT)
type Token = Text
type RefreshToken = Text
type AccessExpiry = NumericDate

data AccessAndRefreshToken 
    = AccessAndRefreshToken 
        { accessToken :: Token
        , refreshToken :: RefreshToken
        } deriving (Show, Generic, ToJSON)


mkJWT :: UTCTime -> JWTsecret -> Int64 -> Token
mkJWT currentTime secret uid =
    let cs =
            mempty -- mempty returns a default JWTClaimsSet
            { iss = stringOrURI "AuthieAtYourService"
            , unregisteredClaims =
                ClaimsMap $
                    Map.fromList [("USERID", Number $ fromIntegral uid)],
                exp = numericDate $ utcTimeToPOSIXSeconds currentTime + (3 * 60) -- 3 minutes
            }
        signer = hmacSecret secret
    in encodeSigned signer mempty cs


mkRefreshToken :: UTCTime -> JWTsecret -> Int64 -> RefreshToken
mkRefreshToken currentTime secret uid =     
    let cs =
            mempty -- mempty returns a default JWTClaimsSet
            {
                unregisteredClaims =
                ClaimsMap $
                    Map.fromList [("USERID", Number $ fromIntegral uid)],
                exp = numericDate $ utcTimeToPOSIXSeconds currentTime + 7 * posixDayLength -- 1 week (ish)
            }
        signer = hmacSecret secret
    in encodeSigned signer mempty cs

    
mkTokens :: UTCTime -> JWTsecret -> Int64 -> AccessAndRefreshToken
mkTokens currentTime secret uid = do
    let accessToken = mkJWT currentTime secret uid
    let refreshToken = mkRefreshToken currentTime secret uid
    AccessAndRefreshToken accessToken refreshToken

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