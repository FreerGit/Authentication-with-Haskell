{-# LANGUAGE OverloadedStrings #-}

module Database.User where

import           Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Int (Int64)
import           Database.Persist (get, insertBy, delete, selectList, Filter, Entity, getBy)
import           Database.Persist.Sql (fromSqlKey, toSqlKey, runSqlPool, toPersistValue)
import           Database.Persist.Postgresql (Entity, ConnectionString, withPostgresqlPool, 
                                             runMigration, SqlPersistT)

import           Database.Schema
import           Authentication.Password
-- import           Models

connString :: ConnectionString
connString = "host=127.0.0.1 port=5432 user=admin dbname=admin password=admin"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a ->  IO a
runAction connString action = runStdoutLoggingT $ withPostgresqlPool connString 10 $
    \backend -> runSqlPool action backend

migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

fetchUserDB :: ConnectionString -> Email -> IO (Maybe (Entity User))
fetchUserDB connString email = do
    let getByEmail = getBy $ UniqueEmail email
    runAction connString getByEmail

fetchUsersDB :: ConnectionString -> IO [Entity User]
fetchUsersDB connString = runAction connString (selectList ([] :: [Filter User]) [])

createUserDB :: ConnectionString -> User -> IO (Maybe Int64)
createUserDB connString user = do
    hashedPassword <- liftIO $ hashPassword $ userPassword user
    let encryptedUser = User (userEmail user) hashedPassword
    maybeUserKey <- runAction connString (insertBy encryptedUser)
    case maybeUserKey of
        Right key -> return $ Just $ fromSqlKey key
        Left _ -> return Nothing

deleteUserDB :: ConnectionString -> Int64 -> IO ()
deleteUserDB connString uid = runAction connString (delete userKey)
    where
        userKey :: Key User
        userKey = toSqlKey uid