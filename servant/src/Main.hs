{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Except
import Data.Aeson
import Data.List ((!?))
import Data.Time.Calendar
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import System.Environment

main :: IO ()
main = do
  port <- read <$> getEnv "SERVANT_PORT"
  putStrLn $ "Serving on http://localhost:" <> show port
  run port app

app :: Application
app = serve (Proxy :: Proxy MyAPI) myServer

data User = User
  { name :: String,
    age :: Int,
    email :: String,
    registration_date :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User

users :: [User]
users =
  [ User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1),
    User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)
  ]

type MyAPI = UsersAPI :<|> Raw

myServer :: Server MyAPI
myServer = usersServer :<|> serveDirectoryWebApp "public"

type APIFor a i =
  Get '[JSON] [a]
    :<|> ReqBody '[JSON] a :> PostNoContent
    :<|> Capture "id" i
      :> ( Get '[JSON] a
             :<|> ReqBody '[JSON] a :> PutNoContent
             :<|> DeleteNoContent
         )

type UsersAPI = "users" :> APIFor User Int

data ServerSpec a i = ServerSpec
  { getAll :: Handler [a],
    add :: a -> Handler NoContent,
    get :: i -> Handler a,
    update :: i -> a -> Handler NoContent,
    delete :: i -> Handler NoContent
  }

serverFor :: ServerSpec a i -> Server (APIFor a i)
serverFor (ServerSpec {getAll, add, get, update, delete}) =
  getAll :<|> add :<|> (\i -> get i :<|> update i :<|> delete i)

usersServer :: Server UsersAPI
usersServer =
  serverFor
    ServerSpec
      { getAll = return users,
        add = \_ -> throwError err503 {errBody = "Not implemented"},
        get = \i -> maybe (throwError err404) return (users !? i),
        update = \_ _ -> throwError err503 {errBody = "Not implemented"},
        delete = \_ -> throwError err503 {errBody = "Not implemented"}
      }
