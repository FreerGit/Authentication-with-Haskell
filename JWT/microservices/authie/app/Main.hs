module Main where

import API (runServer)
import Config
import Control.Monad.Trans.Except (runExceptT)
import Database.User

setupDB :: IO ()
setupDB = do
  env <- runExceptT initialize
  case env of
    Left err -> putStrLn err
    Right res -> migrateDB (getConnString res)

main :: IO ()
main = do
  setupDB
  runServer
