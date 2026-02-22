{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Intro1 where

import Data.Text
import Data.Time (UTCTime)
import Servant.API

type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name

data User = User
  { name :: String,
    age :: Int,
    email :: String,
    registration_date :: UTCTime
  }

type RootEndpoint =
  Get '[JSON] User

type UserAPI2 =
  "users" :> "list-all" :> Get '[JSON] [User]
    :<|> "list-all" :> "users" :> Get '[JSON] [User]

type UserAPI3 = "users" :> "list-all" :> "now" :> Get '[JSON] [User]

-- data Verb method (statusCode :: Nat) (contentType :: [*]) a
-- type Get    = Verb 'GET 200
-- type Delete = Verb 'DELETE 200
-- type Patch  = Verb 'PATCH 200
-- type Post   = Verb 'POST 200
-- type Put    = Verb 'PUT 200

-- type PostCreated  = Verb 'POST 201
-- type PostAccepted = Verb 'POST 202

type UserAPI4 =
  "users" :> Get '[JSON] [User]
    :<|> "admins" :> Get '[JSON] [User]

-- data Stream (method :: k1) (status :: Nat) (framing :: *) (contentType :: *) (a :: *)

-- type StreamGet  = Stream 'GET 200
-- type StreamPost = Stream 'POST 200
-- `NewlineFraming`, `NetstringFraming` and `NoFraming`

type UserAPI5 =
  "user" :> Capture "userid" Integer :> Get '[JSON] User
    :<|> "user" :> Capture "userid" Integer :> DeleteNoContent

-- `QueryParam`, `QueryParams` and `QueryFlag`

type UserAPI6 = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

type UserAPI7 =
  "users" :> ReqBody '[JSON] User :> Post '[JSON] User
    :<|> "users"
      :> Capture "userid" Integer
      :> ReqBody '[JSON] User
      :> Put '[JSON] User

type UserAPI8 = "users" :> Header "User-Agent" Text :> Get '[JSON] [User]

type UserAPI9 = "users" :> Get '[JSON, PlainText, FormUrlEncoded, OctetStream] [User]

type UserAPI10 = "users" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [User])

type ProtectedAPI11 =
  UserAPI -- this is public
    :<|> BasicAuth "my-realm" User :> UserAPI2 -- this is protected by auth

type UserAPI12 innerAPI =
  UserAPI -- this is the fixed bit of the API
    :<|> "inner" :> innerAPI -- this lets us put various other APIs under /inner

type UserAPI12Alone = UserAPI12 EmptyAPI

type UserAPI13 =
  "users" :> Get '[JSON] [User]
    :<|> Raw

type UserAPI15 =
  "files" :> Raw
    :<|> "users" :> Get '[JSON] [User]
