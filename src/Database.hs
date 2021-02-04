{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Int (Int64)
import           Database.Persist (get, insertBy, delete)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Database.Persist.Postgresql (Entity, ConnectionString, withPostgresqlConn, 
                                             runMigration, SqlPersistT)

import           Schema

connString :: ConnectionString
connString = "host=127.0.0.1 port=5432 user=admin dbname=admin password=admin"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a ->  IO a
runAction connString action = runStdoutLoggingT $ withPostgresqlConn connString $ 
    \backend -> runReaderT action backend

migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

fetchUserDB :: ConnectionString -> Int64 -> IO (Maybe User)
fetchUserDB connString uid = runAction connString (get (toSqlKey uid))

createUserDB :: ConnectionString -> User -> IO (Maybe Int64)
createUserDB connString user = do
    maybeUserKey <- runAction connString (insertBy user)
    case maybeUserKey of
        Right key -> return $ Just $ fromSqlKey key
        Left _ -> return Nothing

deleteUserDB :: ConnectionString -> Int64 -> IO ()
deleteUserDB connString uid = runAction connString (delete userKey)
    where
        userKey :: Key User
        userKey = toSqlKey uid