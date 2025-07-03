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
import Lucid.Base (makeAttribute)


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
      with (script_ "") [src_ "/static/htmx.min.js"]
    body_ [class_ "mx-auto max-w-3xl my-4 flex flex-col items-center justify-center"] $ do
      h1_ [class_ "text-3xl font-bold mb-4"] "Todo App"
      p_ [class_ "text-base font-medium mb-6"] "List of things to do"

      -- You could add some placeholder, or load the list dynamically with htmx GET /api
      div_ [id_ "todo-list"] "TODO list will appear here."
      button_ [makeAttribute "hx-get" "/api" , makeAttribute "hx-target" "#todo-list"] "Load Todos"

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