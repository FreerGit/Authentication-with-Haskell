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

module Schema where

import           Data.Aeson
import           Data.Aeson.Types
import           Database.Persist (Entity(..), Entity, keyValueEntityToJSON)
import qualified Database.Persist.TH as PTH
import           Data.Text (Text)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    email Text
    password Text
    UniqueEmail email
    deriving Show Read
|]

instance ToJSON (Entity User) where
    toJSON = keyValueEntityToJSON

instance ToJSON User where 
  toJSON user = object 
    [ "email" .= userEmail user
    , "password" .= userPassword user
    ]

instance FromJSON User where
  parseJSON = withObject "User" parseUser

parseUser :: Object -> Parser User
parseUser o = do
  uEmail <- o .: "email"
  uPassword <- o .: "password"
  return User
    { userEmail = uEmail
    , userPassword = uPassword
    }
