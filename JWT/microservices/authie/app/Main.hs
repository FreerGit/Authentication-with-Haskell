module Main where

import Database.User (connString, migrateDB)
import API (runServer)

setupDB :: IO ()
setupDB = migrateDB connString

main :: IO ()
main = runServer
