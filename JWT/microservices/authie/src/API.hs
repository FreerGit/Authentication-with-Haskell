{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module API where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Database.Persist.Sql
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server
import           Data.Text (Text)

import Database
import Schema
import Cache

type UsersAPI =
         "users" :> Capture "userid" Int64 :> Get '[JSON] User
    :<|> "users" :> Get '[JSON] [Entity User]
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
    :<|> "users" :> "delete" :> Capture "userid" Int64 :> Get '[JSON] ()

type DBInfo = ConnectionString

fetchSpecificUserHandler :: DBInfo -> RedisInfo -> Int64 -> Handler User
fetchSpecificUserHandler connString redisInfo uid = do
    maybeCached <- liftIO $ fetchCachedUser redisInfo uid
    case maybeCached of
        Just user -> return user
        Nothing -> do
            maybeUser <- liftIO $ fetchUserDB connString uid
            case maybeUser of
                Just user -> liftIO $ cacheUser redisInfo uid user >> return user
                Nothing -> Handler $ throwE $ err401 {errBody = "Could not find user"}

fetchUserHandler :: DBInfo -> Handler [Entity User]
fetchUserHandler dbInfo = liftIO $ fetchUsersDB dbInfo
            
createUserHandler :: DBInfo -> User -> Handler Int64
createUserHandler connString user = do
    maybeNewKey <- liftIO $ createUserDB connString user
    case maybeNewKey of
        Just key -> return key
        Nothing -> Handler $ throwE $ err401 {errBody = "Could not create user"}

deleteUserHandler :: DBInfo -> Int64 -> Handler ()
deleteUserHandler connString uid = do
    liftIO $ deleteUserDB connString uid
   
    
usersServer :: DBInfo -> RedisInfo -> Server UsersAPI
usersServer connString redisInfo =
         fetchSpecificUserHandler connString redisInfo
    :<|> fetchUserHandler connString 
    :<|> createUserHandler connString 
    :<|> deleteUserHandler connString 
    

usersAPI :: Proxy UsersAPI
usersAPI = Proxy

runServer :: IO ()
runServer = do
    run 8000 (serve usersAPI (usersServer connString connStringRedis))