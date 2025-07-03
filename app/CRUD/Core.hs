{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module CRUD.Core where

import Network.Wai.Handler.Warp
import Network.Wai
import Servant.Server
import Servant.HTML.Lucid
import Servant.Server.StaticFiles (serveDirectoryFileServer)
import Servant ((:>),Get,JSON,(:<|>)(..),ReqBody,Post,Put,Delete,Capture,Raw)
import Data.Proxy
import CRUD.Query
import Control.Monad.IO.Class
import Lucid


type TodoAPI = Get '[HTML] (Html ()) -- "/" GET [HTML]
                :<|> "todo" :> ReqBody '[JSON] NewTodo :> Post '[JSON] () -- "/todo" Post ()
                :<|> "todo" :> ReqBody '[JSON] EditTodo :> Put '[JSON] () -- "/todo" Put ()
                :<|> "todo-completed" :> ReqBody '[JSON] CompletedTodo :> Put '[JSON] () -- "/todo-completed" Put ()
                :<|> "todo" :> Capture "id" Int :> Delete '[JSON] () -- "/todo/:id" Delete ()

-- Implement UI page
landingPage :: Html ()
landingPage = do
  doctype_
  html_ $ do
    head_ $ do
      title_ "Todo App"
      link_ [rel_ "stylesheet", href_ "/static/output.css"]
      style_ "body { font-family: sans-serif; }"
    body_ $ do
      h1_ "My TODO List"
      h1_ [class_ "text-3xl font-bold text-blue-600"] "Welcome to Tailwind + Haskell!"
      p_ "This is the landing page served with htmx-servant and Lucid."

      -- You could add some placeholder, or load the list dynamically with htmx GET /api
      div_ [id_ "todo-list"] "TODO list will appear here."

-- Server handlers
-- todoUIHandler :: Handler (Html ())
-- todoUIHandler = pure landingPage

fetchTodos :: Handler (Html ())
fetchTodos = do
  _ <- liftIO fetchTodosQuery
  pure landingPage

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

staticFilesServer :: Server ("static" :> Raw)
staticFilesServer = serveDirectoryFileServer "static"

type API = "static" :> Raw
      :<|> TodoAPI

api :: Proxy API
api = Proxy

server :: Server API
server = staticFilesServer :<|> todoAPI

app :: Application
app = serve api server

todoCore :: IO ()
todoCore = do
  putStrLn "server started at port 8080"
  run 8080 app

test :: IO ()
test = putStrLn "Hello from Core"