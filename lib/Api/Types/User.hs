{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Api.Types.User
( User (..)
, Login (..)
) where

import Control.Monad (mzero)
import Data.Aeson ((.=), (.:), FromJSON, ToJSON, Value(..), parseJSON, toJSON, object)

import Api.Types.Fields

-- User type

data User = User
  { user_id        :: UserID
  , user_resourceId :: Maybe ResourceID
  } deriving (Eq, Show)

instance ToJSON User where
  toJSON (User uid rid) =
    object [ "user_id"     .= uid
           , "resource_id" .= rid
           ]

-- Login type

data Login = Login
  { login_userId    :: UserID
  , login_userToken :: UserToken
  } deriving (Eq, Show)

instance FromJSON Login where
  parseJSON (Object v) =
    Login <$> v .: "user_id"
                 <*> v .: "api_token"
  parseJSON _ = mzero

instance ToJSON Login where
  toJSON (Login uid token) =
    object [ "user_id"   .= uid
           , "api_token" .= token
           ]
