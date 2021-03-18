module Cache where

import Control.Monad (void)
import Data.ByteString.Char8 (pack, unpack)
import Data.Int (Int64)
import Database.Redis

import Schema

type RedisInfo = ConnectInfo

connStringRedis :: RedisInfo
connStringRedis = defaultConnectInfo

runRedisAction :: RedisInfo -> Redis a -> IO a
runRedisAction redisInfo action = do
    connection <- connect redisInfo
    runRedis connection action

-- i use void to ignore the result of setex call.
cacheUser :: RedisInfo -> Int64 -> User -> IO ()
cacheUser redisInfo uid user = runRedisAction redisInfo $ void $ setex 
    (pack . show $ uid) 600 (pack . show $ user)

fetchCachedUser :: RedisInfo -> Int64 -> IO (Maybe User)
fetchCachedUser redisInfo uid = runRedisAction redisInfo $ do
    user <- get (pack . show $ uid)
    case user of
        Right (Just found) -> return $ Just (read. unpack $ found)
        _ -> return Nothing