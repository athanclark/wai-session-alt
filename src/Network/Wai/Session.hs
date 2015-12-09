{-# LANGUAGE
    OverloadedStrings
  #-}

{- |
Module      : Network.Wai.Session
Copyright   : (c) 2015 Athan Clark

License     : BSD-3
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC

A simple interface for storing and parsing session information into cookies,
which slightly differs from the <https://hackage.haskell.org/package/wai-session wai-session>
interface designed by Greg Weber.
-}

module Network.Wai.Session where

import Data.Time
import qualified Data.ByteString as BS
import Blaze.ByteString.Builder (toByteString)

import           Data.Vault.Lazy (Key)
import qualified Data.Vault.Lazy as V
import Network.Wai.Trans
import Network.HTTP.Types
import Web.Cookie

import Control.Monad.IO.Class


data SessionConfig m k v = SessionConfig
  { renderKey :: k -> BS.ByteString       -- ^ serialize the key
  , renderVal :: v -> BS.ByteString       -- ^ serialize the value
  , parseKey  :: BS.ByteString -> Maybe k -- ^ parse the serialized key
  , parseVal  :: BS.ByteString -> Maybe v -- ^ parse the serialized value
  , keyName   :: BS.ByteString            -- ^ name used as a cookie
  , valName   :: BS.ByteString            -- ^ name used as a cooke
  , expire    :: Integer                  -- ^ expiration time in Seconds
  , newVal    :: k -> v -> m (Maybe v)    -- ^ method to getting another value -
                                          --   this could ping a nonce cache in @m@
                                          --   for instance.
  , vaultVar  :: Key k                    -- ^ The vault 'Data.Vault.Lazy.Key' used
                                          --   to store the /session/ key
                                          --   when 'newVal' is successful.
  }


sessionMiddleware :: (MonadIO m, Show k, Show v) => SessionConfig m k v -> MiddlewareT m
sessionMiddleware cfg app req respond = do
  liftIO $ putStrLn $ "Req Headers: " ++ show (parseSessionCookies cfg $ requestHeaders req)
  case parseSessionCookies cfg (requestHeaders req) of
    Nothing        -> app req respond
    Just (key,val) -> do
      liftIO $ putStrLn "Parsed!"
      mVal <- newVal cfg key val
      case mVal of
        Nothing    -> app req respond
        Just val'  ->
          let f    = mapResponseHeaders (++ renderSessionCookies cfg key val')
              req' = req {vault = V.insert (vaultVar cfg) key (vault req)}
          in app req' (respond . f)


parseSessionCookies :: SessionConfig m k v -> RequestHeaders -> Maybe (k, v)
parseSessionCookies cfg xs = do
  cookies <- parseCookies <$> lookup "Cookie" xs
  key     <- parseKey cfg =<< lookup (keyName cfg) cookies
  val     <- parseVal cfg =<< lookup (valName cfg) cookies
  return (key, val)

renderSessionCookies :: SessionConfig m k v -> k -> v -> ResponseHeaders
renderSessionCookies cfg key val = repeat "Set-Cookie" `zip` cookies
  where
    cookies = (toByteString . renderSetCookie) <$>
      [ def { setCookieName   = keyName cfg
            , setCookieValue  = renderKey cfg key
            , setCookieMaxAge = Just $ secondsToDiffTime (expire cfg)
            }
      , def { setCookieName   = valName cfg
            , setCookieValue  = renderVal cfg val
            , setCookieMaxAge = Just $ secondsToDiffTime (expire cfg)
            }
      ]

