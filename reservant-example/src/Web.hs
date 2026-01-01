{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Web
  ( startServer,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, FromRow, Only (Only), ToRow, execute, execute_, lastInsertRowId, open, query, query_)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant

-- This file contains an example cats API
-- SQLite is used for simplicity

-- The Cat datatype
data Cat = Cat
  { name :: T.Text,
    age :: Int32,
    breed :: Maybe T.Text
  }
  deriving (Show, Generic)

instance FromJSON Cat

instance ToJSON Cat

instance FromRow Cat

instance ToRow Cat

-- Initial values in the database
cats :: [Cat]
cats =
  [ Cat "Kitty" 2 (Just "British Shorthair"),
    Cat "Felix" 3 Nothing,
    Cat "Goobert" 1 (Just "Scottish Straight")
  ]

-- Commands to initialize the SQLite database
initializeDb :: Connection -> IO ()
initializeDb conn = do
  _ <-
    execute_
      conn
      "create table if not exists cats (name TEXT REQUIRED PRIMARY KEY, age INTEGER REQUIRED, breed TEXT)"

  forM_ cats $ \cat -> do
    execute
      conn
      "insert into cats (name, age, breed) values (?, ?, ?)"
      (cat :: Cat)

  putStrLn "Initialized DB"

-- API endpoints definitions
type GetCats = Get '[JSON] [Cat]

type GetCatByName = Capture "name" T.Text :> Get '[JSON] Cat

type AddCat = ReqBody '[JSON] Cat :> PostCreated '[JSON] Cat

type CatsAPI =
  "cats"
    :> ( GetCats
           :<|> GetCatByName
           :<|> AddCat
       )

-- API endpoint handlers
getCats :: Connection -> Handler [Cat]
getCats conn = do
  liftIO $ query_ conn "select * from cats" :: Handler [Cat]

addCat :: Connection -> Cat -> Handler Cat
addCat conn newCat = do
  liftIO $
    execute
      conn
      "insert into cats (name, age, breed) values (?, ?, ?)"
      newCat

  lastRowId <- liftIO $ lastInsertRowId conn

  [cat] <-
    liftIO $
      query conn "select * from cats where ROWID = (?)" (Only lastRowId) ::
      Handler [Cat]

  return cat

getCatByName :: Connection -> T.Text -> Handler Cat
getCatByName conn catName = do
  catList <-
    liftIO $
      query
        conn
        "select * from cats where lower(name) = (?)"
        (Only $ T.toLower catName) ::
      Handler [Cat]

  case catList of
    [] -> throwError err404
    cat : _ -> return cat

api :: Proxy CatsAPI
api = Proxy

server :: Connection -> Server CatsAPI
server conn = getCats conn :<|> getCatByName conn :<|> addCat conn

-- Server application entry point
startServer :: IO ()
startServer = do
  conn <- open "cats.db"

  -- uncomment below line on first run to initialize the database
  -- initializeDb conn

  run 8081 $ serve api (server conn)