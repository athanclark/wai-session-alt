{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , DeriveGeneric
  , DeriveDataTypeable
  , MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
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

import Network.Wai.Trans
import Network.HTTP.Types
import Web.Cookie
import Control.Monad.Trans

import GHC.Generics
import Data.Typeable


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
  }


sessionMiddleware :: Monad m => SessionConfig m k v
                             -> ApplicationT (SessionT k m)
                             -> ApplicationT m
sessionMiddleware cfg app req respond = do
  case parseSessionCookies cfg (requestHeaders req) of
    Nothing        -> hoistApplicationT (`runSessionT` Nothing) lift app req respond
    Just (key,val) -> do
      mVal <- newVal cfg key val
      case mVal of
        Nothing    -> hoistApplicationT (`runSessionT` Nothing) lift app req respond
        Just val'  -> let f = mapResponseHeaders (++ renderSessionCookies cfg key val')
                      in  hoistApplicationT (`runSessionT` Just key) lift app req (respond . f)


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


-- | May or may not have a current session
newtype SessionT k m a = SessionT
  { runSessionT :: Maybe k -> m a
  } deriving (Functor, Generic, Typeable)

instance Applicative m => Applicative (SessionT k m) where
  pure = SessionT . const . pure
  (SessionT f) <*> (SessionT x) =
    SessionT $ \s -> f s <*> x s

instance Monad m => Monad (SessionT k m) where
  return = pure
  (SessionT x) >>= f = SessionT $ \s ->
    x s >>= (flip runSessionT s . f)

instance MonadTrans (SessionT k) where
  lift = SessionT . const


class MonadSession k m | m -> k where
  currentSession :: m (Maybe k)

instance Applicative m => MonadSession k (SessionT k m) where
  currentSession = SessionT $ \s -> pure s
