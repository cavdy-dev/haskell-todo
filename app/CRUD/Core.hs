{-# LANGUAGE DataKinds #-}
module CRUD.Core where

import Network.Wai.Handler.Warp
import Network.Wai
import Servant.Server
import Servant ((:>),Get,JSON,(:<|>)(..),ReqBody,Post,Put,Delete,Capture)
import Data.Proxy
import CRUD.Query
import Control.Monad.IO.Class

type TodoAPI = "todos" :> Get '[JSON] [Todo] -- "/todos" GET [TODO]
                :<|> "todo" :> ReqBody '[JSON] NewTodo :> Post '[JSON] () -- "/todo" Post ()
                :<|> "todo" :> ReqBody '[JSON] EditTodo :> Put '[JSON] () -- "/todo" Put ()
                :<|> "todo-completed" :> ReqBody '[JSON] CompletedTodo :> Put '[JSON] () -- "/todo-completed" Put ()
                :<|> "todo" :> Capture "id" Int :> Delete '[JSON] () -- "/todo/:id" Delete ()

fetchTodos :: Handler [Todo]
fetchTodos = do
  todoList <- liftIO fetchTodosQuery
  return todoList

createTodo :: NewTodo -> Handler ()
createTodo todo = do
  liftIO $ createTodoQuery todo
  pure ()

updateTodo :: EditTodo -> Handler ()
updateTodo todo = do
  liftIO $ updateTodoQuery todo
  pure ()

completedTodo :: CompletedTodo -> Handler ()
completedTodo todo = do
  liftIO $ completedTodoQuery todo
  pure ()

deleteTodo :: Int -> Handler ()
deleteTodo todoId = do
  liftIO $ deleteTodoQuery todoId
  pure ()

todoAPI :: Server TodoAPI
todoAPI = (fetchTodos :<|> createTodo :<|> updateTodo :<|> completedTodo :<|> deleteTodo)

app :: Application
app = serve (Proxy :: Proxy TodoAPI) todoAPI

todoCore :: IO ()
todoCore = do
  putStrLn "server started at port 8080"
  run 8080 app

test :: IO ()
test = putStrLn "Hello from Core"