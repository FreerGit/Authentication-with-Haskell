{-#LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Text
import Control.Monad.Except (ExceptT (..), throwError, liftIO)
import LoadEnv
import System.Envy
import GHC.Generics
import System.Environment (lookupEnv)
import Database.Persist.Postgresql (ConnectionString)

-- connString = "host=127.0.0.1 port=5432 user=admin dbname=admin password=admin"

type DBInfo = ConnectionString
type JWTsecret = Text
type RefreshSecret = Text

data Env 
    = Env
    { connString :: DBInfo  
    , jwtSecret :: JWTsecret
    } deriving (Generic, Show)

type Init a = ExceptT String IO a

instance FromEnv Env

getConnString :: Env -> DBInfo
getConnString = connString

getJWTSecret :: Env -> JWTsecret
getJWTSecret = jwtSecret

loadConfig :: Init Env
loadConfig = ExceptT $ liftIO $ loadEnvFrom ".env" >> decodeEnv

-- Include a connection pool here later
initialize :: Init Env
initialize = do loadConfig