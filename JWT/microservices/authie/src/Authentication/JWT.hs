{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Authentication.JWT where

import qualified Data.Map                       as Map
import qualified Data.ByteString.Lazy.Internal  as BS
import qualified Data.ByteString.Lazy.Char8     as LB
import qualified Data.Text.Encoding             as T

import           Data.Text
import           Web.JWT
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX
import           Data.Aeson (ToJSON)
import           Control.Monad (guard)
import           Data.Aeson.Types (Value (Bool, Number))
import           Data.Scientific  (coefficient)
import           Prelude hiding (exp)
import           Data.Int (Int64)
import           Data.Digest.Pure.SHA
import           GHC.Generics

import           Config

type Token = Text
type RefreshToken = Text
type AccessExpiry = NumericDate

data AccessAndRefreshToken 
    = AccessAndRefreshToken 
        { accessToken :: Token
        , refreshToken :: RefreshToken
        , refreshTokenExp :: UTCTime
        } deriving (Show, Generic, ToJSON)


mkJWT :: UTCTime -> JWTsecret -> Int64 -> Token
mkJWT currentTime secret uid =
    let cs =
            mempty -- mempty returns a default JWTClaimsSet
            { iss = stringOrURI "AuthieAtYourService"
            , unregisteredClaims =
                ClaimsMap $
                    Map.fromList [("USERID", Number $ fromIntegral uid)],
                exp = numericDate $ utcTimeToPOSIXSeconds currentTime + (5 * 60) -- 5 minutes
            }
        signer = hmacSecret secret
    in encodeSigned signer mempty cs


mkRefreshToken :: POSIXTime -> JWTsecret -> Int64 -> (RefreshToken, POSIXTime)
mkRefreshToken expiry secret uid = do
    let cs =
            mempty -- mempty returns a default JWTClaimsSet
            {
                unregisteredClaims =
                ClaimsMap $
                    Map.fromList [("USERID", Number $ fromIntegral uid)],
                exp = numericDate expiry
            }
        signer = hmacSecret secret
    (encodeSigned signer mempty cs, expiry)

    -- encodeSigned signer mempty cs, expiry

    
mkTokens :: UTCTime -> JWTsecret -> Int64 -> AccessAndRefreshToken
mkTokens currentTime secret uid = do
    let posix = utcTimeToPOSIXSeconds currentTime + 7 * posixDayLength -- 7 days
    let accessToken = mkJWT currentTime secret uid
    let (refreshToken, expiry) = mkRefreshToken posix secret uid
    AccessAndRefreshToken accessToken refreshToken (posixSecondsToUTCTime expiry)


verifyJWT :: UTCTime -> JWTsecret -> Token -> Maybe Int
verifyJWT currentTime secret token = do
  let signer = hmacSecret secret
  unverifiedJWT <- decode token
  verifiedJWT <- verify signer unverifiedJWT
  expTime <- exp . claims $ verifiedJWT
  now <- numericDate $ utcTimeToPOSIXSeconds currentTime
  guard (now < expTime) -- if expired, reject
  let kv = unClaimsMap . unregisteredClaims . claims $ verifiedJWT
  userIDVal <- Map.lookup "USERID" kv
  case userIDVal of
    Number userID -> return . fromIntegral $ coefficient userID
    _ -> Nothing