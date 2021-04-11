{-# LANGUAGE OverloadedStrings #-}

module Database.UserCache where

import           Control.Monad (void)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Int (Int64)
import           Database.Redis
import qualified Data.Text as T
import           Database.Schema

type RedisInfo = ConnectInfo

connStringRedis :: RedisInfo
connStringRedis = defaultConnectInfo

runRedisAction :: RedisInfo -> Redis a -> IO a
runRedisAction redisInfo action = do
    connection <- connect redisInfo
    runRedis connection action

-- i use void to ignore the result of setex call.
cacheUser :: RedisInfo -> Email -> User -> IO ()
cacheUser redisInfo email user = runRedisAction redisInfo $ void $ setex 
    (pack . show $ email) 600 (pack . show $ user)

fetchCachedUser :: RedisInfo -> Email -> IO (Maybe User)
fetchCachedUser redisInfo email = runRedisAction redisInfo $ do
    user <- get (pack . show $ email)
    case user of
        Right (Just found) -> return $ Just (read. unpack $ found)
        _ -> return Nothing