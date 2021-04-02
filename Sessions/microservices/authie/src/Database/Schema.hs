{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}

module Database.Schema where

import           Data.Aeson
import           Data.Aeson.Types
import           Database.Persist (Entity(..), Entity, keyValueEntityToJSON)
import qualified Database.Persist.TH as PTH
import           Data.Text (Text)


-- import           Models

-- With the help from template haskell, the json statement essentially produces
-- the commented code below, neat.
PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistUpperCase|
  User sql=users json
    email Text
    password Text
    UniqueEmail email
    deriving Show Read
|]

type Email = Text

-- instance ToJSON (Entity User) where
--     toJSON = keyValueEntityToJSON

-- instance ToJSON User where 
--     toJSON user = object 
--         [ "email" .= userEmail user
--         , "password" .= userPassword user
--         ]

-- instance FromJSON User where
--   parseJSON = withObject "User" parseUser

-- parseUser :: Object -> Parser User
-- parseUser o = do
--     uEmail <- o .: "email"
--     uPassword <- o .: "password"
--     return User
--         { userEmail = uEmail
--         , userPassword = uPassword
--         }
