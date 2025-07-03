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
import qualified Data.Text as T

data Todo = Todo {
  todoId :: Int
  , title :: Text
} deriving (Eq,Show,FromRow,ToRow,Generic,ToJSON,FromJSON)

data NewTodo = NewTodo {
  newTitle :: Text
} deriving (Eq,Show,FromRow,ToRow,Generic,ToJSON,FromJSON)

data EditTodo = EditTodo {
  editTitle :: Text
  ,editId :: Text
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
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS todos (id SERIAL PRIMARY KEY, title VARCHAR)"

  pure conn

fetchTodosQuery :: IO [Todo]
fetchTodosQuery = do
  conn <- getConn
  todos <- query_ conn "SELECT id, title FROM todos;"
  close conn
  pure todos

createTodoQuery :: NewTodo -> IO Todo
createTodoQuery todo = do
  conn <- getConn
  results <- query conn "INSERT INTO todos (title) VALUES (?) RETURNING id, title;" (Only (newTitle todo))
  close conn
  case results of
    [todo'] -> pure todo'
    []      -> error "No todo returned from INSERT"
    _       -> error "Multiple todos returned from INSERT"

updateTodoQuery :: EditTodo -> IO ()
updateTodoQuery todo = do
  let tId = read (T.unpack (editId todo)) :: Int
  conn <- getConn
  _ <- execute conn "UPDATE todos SET title = ? WHERE id = ?;" (editTitle todo, tId)
  close conn

deleteTodoQuery :: Int -> IO ()
deleteTodoQuery todoId = do
  conn <- getConn
  _ <- execute conn "DELETE FROM todos WHERE id = ?" (Only todoId)
  close conn

test :: IO ()
test = putStrLn "Hello from Query"