module Main where

import Control.Monad.Trans.Except (runExceptT)

import Database.User
import API (runServer)
import Config

setupDB :: IO ()
setupDB = do
    env <- runExceptT initialize
    case env of
        Left err -> putStrLn err
        Right res -> migrateDB (getConnString res)

main :: IO ()
main = runServer
