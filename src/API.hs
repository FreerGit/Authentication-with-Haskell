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

type UsersAPI =
         "users" :> Capture "userid" Int64 :> Get '[JSON] User
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
    :<|> "users" :> "delete" :> Capture "userid" Int64 :> Get '[JSON] ()


fetcherUserHandler :: ConnectionString -> Int64 -> Handler User
fetcherUserHandler connString uid = do
    maybeUser <- liftIO $ fetchUserDB connString uid
    case maybeUser of
        Just user -> return user
        Nothing -> Handler $ (throwE $ err401 {errBody = "Could not find user"})

createUserHandler :: ConnectionString -> User -> Handler Int64
createUserHandler connString user = do
    maybeNewKey <- liftIO $ createUserDB connString user
    case maybeNewKey of
        Just key -> return key
        Nothing -> Handler $ (throwE $ err401 {errBody = "Could not create user"})

deleteUserHandler :: ConnectionString -> Int64 -> Handler ()
deleteUserHandler connString uid = do
    maybeUser <- liftIO $ deleteUserDB connString uid
    return maybeUser
    
usersServer :: ConnectionString -> Server UsersAPI
usersServer connString =
         (fetcherUserHandler connString) 
    :<|> (createUserHandler connString)
    :<|> (deleteUserHandler connString)
    

usersAPI :: Proxy UsersAPI
usersAPI = Proxy

runServer :: IO ()
runServer = do
    run 8000 (serve usersAPI (usersServer connString))