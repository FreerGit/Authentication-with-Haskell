{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module API where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Data.Int (Int64)
import Data.Proxy (Proxy(..))
import Database.Persist (Key, Entity)
import Database.Persist.Sql
import Database.Persist.Postgresql (ConnectionString)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Client
import Servant.Server
import Data.Text (Text)

import Database.User
import Database.Schema
import Database.UserCache
import Authentication.Password

type UsersAPI =
         "v1" :> "register" :> ReqBody '[JSON] User :> Post '[JSON] Int64
    :<|> "v1" :> "users"    :> Get '[JSON] [Entity User]
    :<|> "v1" :> "login"    :> ReqBody '[JSON] User :> Get '[JSON] Bool

    -- :<|> "users" :> "delete" :> Capture "userid" Int64 :> Get '[JSON] ()

type DBInfo = ConnectionString

loginHandler :: DBInfo  -> User -> Handler Bool
loginHandler connString user = do
    maybeUser <- liftIO $ fetchUserDB connString (userEmail user)
    case maybeUser of
        Just foundUser -> return $ validateHashedPassword (userPassword $ entityVal foundUser) (userPassword user)
        Nothing -> Handler $ throwE $ err401 {errBody = "Could not find user"}

fetchUserHandler :: DBInfo -> Handler [Entity User]
fetchUserHandler dbInfo = liftIO $ fetchUsersDB dbInfo
            
registerHandler :: DBInfo -> User -> Handler Int64
registerHandler connString user = do
    maybeNewKey <- liftIO $ createUserDB connString user
    case maybeNewKey of
        Just key -> return key
        Nothing -> Handler $ throwE $ err401 {errBody = "Could not create user"}

deleteUserHandler :: DBInfo -> Int64 -> Handler ()
deleteUserHandler connString uid = do
    liftIO $ deleteUserDB connString uid
   
    
usersServer :: DBInfo -> Server UsersAPI
usersServer connString =
         registerHandler connString 
    :<|> fetchUserHandler connString 
    :<|> loginHandler connString
    -- :<|> deleteUserHandler connString 
    

usersAPI :: Proxy UsersAPI
usersAPI = Proxy

runServer :: IO ()
runServer = do
    run 8000 (serve usersAPI (usersServer connString))