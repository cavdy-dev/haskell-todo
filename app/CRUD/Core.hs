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
import qualified Data.Text as T
import Data.Text (Text)


type TodoAPI = Get '[HTML] (Html ()) -- "/" GET [HTML]
                :<|> "todos" :> Get '[HTML] (Html ()) -- "/todos" Post ()
                :<|> "todo" :> ReqBody '[JSON] NewTodo :> Post '[JSON] () -- "/todo" Post ()
                :<|> "todo" :> ReqBody '[JSON] EditTodo :> Put '[JSON] () -- "/todo" Put ()
                :<|> "todo-completed" :> ReqBody '[JSON] CompletedTodo :> Put '[JSON] () -- "/todo-completed" Put ()
                :<|> "todo" :> Capture "id" Int :> Delete '[JSON] () -- "/todo/:id" Delete ()

-- Implement UI page
todoItem :: Int -> Text -> Html ()
todoItem todoId name = 
  li_ [ class_ "flex items-center justify-between gap-x-4 py-2 w-lg border border-gray-300 rounded px-4" ] $ do
    p_ [ class_ "text-sm/6 font-semibold text-gray-900" ] (toHtml name)
    button_ 
      [ class_ "border border-red-500 text-red-500 px-3 py-1 h-full cursor-pointer rounded"
      , makeAttribute "hx-post" ("/api/" <> T.pack (show todoId))
      ] "Delete"

todoList :: [Todo] -> Html ()
todoList todos = do
  ul_ [role_ "list", class_ "divide-y divide-gray-200 my-6", id_ "todos"] $ do
    mapM_ (\(Todo tid name _) -> todoItem tid name) todos

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

      form_ [class_ "w-full bg-gray-200 flex items-center w-sm h-12 rounded-lg", makeAttribute "hx-post" "/todo"] $ do
        input_ [id_ "todo-input", class_ "w-full h-full rounded-s-lg px-2 outline-none border-none", placeholder_ "Enter todo", type_ "text", name_ "newTitle"]
        button_ [class_ "bg-red-500 text-white px-4 h-full cursor-pointer rounded-e-lg"] "Add"

      with (div_ mempty)
        [ makeAttribute "hx-get" "/todos"
        , makeAttribute "hx-trigger" "load"
        , makeAttribute "hx-target" "#result"
        , makeAttribute "hx-swap" "innerHTML"
        ]

      with (div_ mempty)
        [ id_ "result"]

entryPoint :: Handler (Html ())
entryPoint = do
  pure landingPage

fetchTodos :: Handler (Html ())
fetchTodos = do
  todos <- liftIO fetchTodosQuery
  pure (todoList todos)

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
todoAPI = (entryPoint :<|> fetchTodos :<|> createTodo :<|> updateTodo :<|> completedTodo :<|> deleteTodo)

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