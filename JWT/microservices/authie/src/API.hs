{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Authentication.JWT
import Authentication.Password
import Config
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson (encode)
import Data.Aeson.Types
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Clock (getCurrentTime)
import Database.Persist (Entity, Key)
import Database.Persist.Sql
import Database.Schema
import Database.User
import Database.UserCache
import GHC.Generics
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Errors (HasErrorBody (..), errorMw)
import Servant.API
import Servant.Client
import Servant.Server
import Web.Cookie

type UsersAPI =
  "v1" :> "register" :> ReqBody '[JSON] User :> Post '[JSON] Text
    :<|> "v1" :> "users" :> Get '[JSON] [Entity User]
    :<|> "v1" :> "login" :> ReqBody '[JSON] User :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] Token)
    :<|> "v1" :> "refreshToken" :> Header "Cookie" Text :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] Token)

-- :<|> "users" :> "delete" :> Capture "userid" Int64 :> Get '[JSON] ()

-- encodeErrorResponse :: Text -> ServantErr -> Handler ()
encodeErrorResponse body errFunc = do
  errFunc
    { errBody = encode (body :: Text),
      errHeaders = [("Content-Type", "application/json")]
    }

---------------------------------------------------------------------------------

registerHandler :: Env -> User -> Handler Text
registerHandler env user = do
  let connString = getConnString env
  maybeNewKey <- liftIO $ createUserDB connString user
  case maybeNewKey of
    Just key -> return "OK"
    Nothing -> Handler $ throwE $ encodeErrorResponse "Could not create user" err401

---------------------------------------------------------------------------------

fetchUserHandler :: DBInfo -> Handler [Entity User]
fetchUserHandler dbInfo = liftIO $ fetchUsersDB dbInfo

loginHandler :: Env -> User -> Handler (Headers '[Header "Set-Cookie" SetCookie] Token)
loginHandler env user = do
  let connString = getConnString env
  maybeUser <- liftIO $ fetchUserDB connString (userEmail user)
  case maybeUser of
    Just foundUser -> do
      time <- liftIO getCurrentTime
      let secret = getJWTSecret env
      if validateHashedPassword (userPassword $ entityVal foundUser) (userPassword user)
        then do
          let tokens = mkTokens time secret (fromSqlKey $ entityKey foundUser)
          let cookie =
                defaultSetCookie
                  { setCookieName = "ref_token",
                    setCookieValue = encodeUtf8 $ refreshToken tokens,
                    setCookieHttpOnly = True,
                    setCookieExpires = Just $ refreshTokenExp tokens
                  }
          return $ addHeader cookie (accessToken tokens)
        else Handler $ throwE $ encodeErrorResponse "No such user" err402
    Nothing -> Handler $ throwE $ encodeErrorResponse "No such user" err402

---------------------------------------------------------------------------------

refreshTokenHandler :: Env -> Maybe Text -> Handler (Headers '[Header "Set-Cookie" SetCookie] Token)
refreshTokenHandler env (Just refreshCookie) = do
  let extractCookie = parseCookies $ encodeUtf8 refreshCookie
  let extractedToken = decodeUtf8 $ snd $ head extractCookie -- FIX, unsafe head
  time <- liftIO getCurrentTime
  let secret = getJWTSecret env
  let maybeValidateToken = verifyJWT time secret extractedToken
  case maybeValidateToken of
    Just uid -> do
      let tokens = mkTokens time secret $ fromIntegral uid
      let cookie =
            defaultSetCookie
              { setCookieName = "ref_token",
                setCookieValue = encodeUtf8 $ refreshToken tokens,
                setCookieHttpOnly = True,
                setCookieExpires = Just $ refreshTokenExp tokens
              }
      return $ addHeader cookie (accessToken tokens)
    Nothing -> Handler $ throwE $ encodeErrorResponse "refresh token expired" err402
refreshTokenHandler env Nothing = Handler $ throwE $ encodeErrorResponse "No refresh token" err402

---------------------------------------------------------------------------------

-- deleteUserHandler :: Env -> Int64 -> Handler ()
-- deleteUserHandler env uid = do
--     liftIO $ deleteUserDB connString uid

usersServer :: Env -> Server UsersAPI
usersServer env =
  registerHandler env
    :<|> fetchUserHandler (getConnString env)
    :<|> loginHandler env
    :<|> refreshTokenHandler env

-- :<|> deleteUserHandler connString

usersAPI :: Proxy UsersAPI
usersAPI = Proxy

allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  simpleCorsResourcePolicy
    { corsOrigins = Just (["http://localhost:8080"], True), -- null for now since localhost
      corsMethods = ["OPTIONS", "GET", "PUT", "POST"],
      corsRequestHeaders = ["Authorization", "Content-Type", "Cache-Control"]
    }

runServer :: IO ()
runServer = do
  env <- runExceptT initialize
  case env of
    Left err -> putStrLn ("Error: " ++ err)
    Right res -> do
      putStrLn "Starting server!"
      run 8000 $ allowCors $ errorMw @JSON @["error", "status"] $ serve usersAPI (usersServer res)