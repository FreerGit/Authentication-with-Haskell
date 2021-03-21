module Main where

import Database (connString, migrateDB)
import API (runServer)

setupDB :: IO ()
setupDB = migrateDB connString

main :: IO ()
main = runServer
