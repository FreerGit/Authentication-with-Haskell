{-# LANGUAGE OverloadedStrings  #-}

module Authentication.Session where

import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.Base64  as XD

import Web.ClientSession
import Control.Monad.Trans.Except (throwE, runExceptT)
import Control.Monad.IO.Class (liftIO)

import Config

encryptSessionIO :: Env -> T.Text -> IO BS.ByteString
encryptSessionIO env toEncrypt = do
    key <- liftIO $ getKeyEnv "SECRET_KEY"
    encryptIO key "fdsfds"