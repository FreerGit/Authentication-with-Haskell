{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module API where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE, runExceptT)
import Data.Int (Int64)
import Data.Proxy (Proxy(..))
import Database.Persist (Key, Entity)
import Database.Persist.Sql
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Network.Wai (Middleware)
-- import Network.Wai.Middleware.Servant.Options
import Servant.API
import Servant.Client
import Servant.Server
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)

import Database.User
import Database.Schema
import Database.UserCache
import Authentication.Password
import Authentication.JWT
import Config

type UsersAPI =
         "v1" :> "register" :> ReqBody '[JSON] User :> Post '[JSON] Int64
    :<|> "v1" :> "users"    :> Get '[JSON] [Entity User]
    :<|> "v1" :> "login"    :> ReqBody '[JSON] User :> Post '[JSON] Token

    -- :<|> "users" :> "delete" :> Capture "userid" Int64 :> Get '[JSON] ()

loginHandler :: Env -> User -> Handler Token
loginHandler env user = do
    let connString = getConnString env
    maybeUser <- liftIO $ fetchUserDB connString (userEmail user)
    case maybeUser of
        Just foundUser -> do
            time <- liftIO getCurrentTime
            let secret = getJWTSecret env
            if validateHashedPassword (userPassword $ entityVal foundUser) (userPassword user)
                then return $ mkJWT time secret (fromSqlKey $ entityKey foundUser)
                else Handler $ throwE $ err402 {errBody = "Could not make JWT"}
        Nothing -> Handler $ throwE $ err401 {errBody = "Could not find user"}

fetchUserHandler :: DBInfo -> Handler [Entity User]
fetchUserHandler dbInfo = liftIO $ fetchUsersDB dbInfo
            
registerHandler :: DBInfo -> User -> Handler Int64
registerHandler dbInfo user = do
    maybeNewKey <- liftIO $ createUserDB dbInfo user
    case maybeNewKey of
        Just key -> return key
        Nothing -> Handler $ throwE $ err401 {errBody = "Could not create user"}

-- deleteUserHandler :: Env -> Int64 -> Handler ()
-- deleteUserHandler env uid = do
--     liftIO $ deleteUserDB connString uid
   
    

usersServer :: Env -> Server UsersAPI
usersServer env =
         registerHandler (getConnString env) 
    :<|> fetchUserHandler (getConnString env)
    :<|> loginHandler env
    -- :<|> deleteUserHandler connString 
    

usersAPI :: Proxy UsersAPI
usersAPI = Proxy

allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
    simpleCorsResourcePolicy
        { corsMethods = ["OPTIONS", "GET", "PUT", "POST"]
        , corsRequestHeaders = ["Content-Type"]
        }

runServer :: IO ()
runServer = do
    env <- runExceptT initialize
    case env of
        Left err -> putStrLn err
        Right res -> run 8000 $ allowCors $ serve usersAPI (usersServer res)