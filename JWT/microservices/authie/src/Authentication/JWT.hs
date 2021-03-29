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
import          GHC.Generics
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
                exp = numericDate $ utcTimeToPOSIXSeconds currentTime + 30 * posixDayLength
            }
        signer = hmacSecret secret
    in encodeSigned signer mempty cs


-- fastWrapper = do
--     env <- runExceptT initialize
--     case env of
--         Left err -> LB.putStrLn "fds"
--         Right res ->  do
--             let x = getJWTSecret res
--             let r = getRefreshSecret res
--             mkRefreshToken (LB.fromChunks . return . T.encodeUtf8 $ x) 5


-- mkRefreshToken (LB.fromChunks . return . T.encodeUtf8 $ x) 5
-- mkRefreshToken :: JWTsecret -> Int64 -> RefreshToken
mkRefreshToken :: JWTsecret -> Int64 -> RefreshToken
mkRefreshToken secret uid = do
    let key = LB.fromChunks . return . T.encodeUtf8 $ secret
    let digest = hmacSha256 key $ BS.packChars  ("refresh_token=" ++ (show uid)) 
    pack $ showDigest digest

    
mkTokens :: UTCTime -> JWTsecret -> Int64 -> AccessAndRefreshToken
mkTokens currentTime secret uid = do
    let accessToken = mkJWT currentTime secret uid
    let refreshToken = mkRefreshToken secret uid
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