{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CRUD.Query where

import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (getEnv)
import Database.PostgreSQL.Simple
import GHC.Generics
import Data.Aeson
import Data.Text (Text)

data Todo = Todo {
  id :: Int
  , title :: Text
  , completed :: Bool
} deriving (Eq,Show,FromRow,ToRow,Generic,ToJSON,FromJSON)

data NewTodo = NewTodo {
  newTitle :: Text
} deriving (Eq,Show,FromRow,ToRow,Generic,ToJSON,FromJSON)

data EditTodo = EditTodo {
  editTitle :: Text
  ,editId :: Int
} deriving (Eq,Show,FromRow,ToRow,Generic,ToJSON,FromJSON)

data CompletedTodo = CompletedTodo {
  completedState :: Bool
  ,completedId :: Int
} deriving (Eq,Show,FromRow,ToRow,Generic,ToJSON,FromJSON)

getConn :: IO Connection
getConn = do
  loadFile defaultConfig

  -- Access variables
  host <- getEnv "PGHOST"
  user <- getEnv "PGUSER"
  password <- getEnv "PGPASSWORD"
  database   <- getEnv "PGDATABASE"

  -- Database connection
  conn <- connect defaultConnectInfo { 
    connectHost = host, connectDatabase = database, connectUser = user, connectPassword = password
    }

  -- Create todo table if not exist
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS todos (id SERIAL PRIMARY KEY, title VARCHAR, completed BOOLEAN)"

  pure conn

fetchTodosQuery :: IO [Todo]
fetchTodosQuery = do
  conn <- getConn
  todos <- query_ conn "SELECT * FROM todos;"
  close conn
  pure todos

createTodoQuery :: NewTodo -> IO ()
createTodoQuery todo = do
  conn <- getConn
  _ <- execute conn "INSERT INTO todos (title, completed) VALUES (?, FALSE);" (Only (newTitle todo))
  close conn

updateTodoQuery :: EditTodo -> IO ()
updateTodoQuery todo = do
  conn <- getConn
  _ <- execute conn "UPDATE todos SET title = ? WHERE id = ?;" (editTitle todo, editId todo)
  close conn

completedTodoQuery :: CompletedTodo -> IO ()
completedTodoQuery todo = do
  conn <- getConn
  _ <- execute conn "UPDATE todos SET completed = ? WHERE id = ?;" (completedState todo, completedId todo)
  close conn

deleteTodoQuery :: Int -> IO ()
deleteTodoQuery todoId = do
  conn <- getConn
  _ <- execute conn "DELETE FROM todos WHERE id = ?" (Only todoId)
  close conn

test :: IO ()
test = putStrLn "Hello from Query"