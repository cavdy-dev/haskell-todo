{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module CRUD.Core where

import Network.Wai.Handler.Warp
import Network.Wai
import Servant.Server
import Servant.HTML.Lucid
import Servant.Server.StaticFiles (serveDirectoryFileServer)
import Servant ((:>),Get,JSON,(:<|>)(..),ReqBody,Post,Patch,Delete,Capture,Raw,NoContent(..))
import Data.Proxy
import CRUD.Query
import Control.Monad.IO.Class
import Lucid
import Lucid.Base (makeAttribute)
import qualified Data.Text as T
import Data.Text (Text)


type TodoAPI = Get '[HTML] (Html ()) -- "/" GET [HTML]
                :<|> "todos" :> Get '[HTML] (Html ()) -- "/todos" Post ()
                :<|> "todo" :> ReqBody '[JSON] NewTodo :> Post '[HTML] (Html ()) -- "/todo" Post ()
                :<|> "todo" :> ReqBody '[JSON] EditTodo :> Patch '[JSON] () -- "/todo" Patch ()
                :<|> "todo" :> Capture "id" Int :> Delete '[JSON] NoContent -- "/todo/:id" Delete ()

-- Implement UI page
todoItem :: Int -> Text -> Html ()
todoItem todoId title = 
  li_ [ class_ "flex items-center justify-between gap-x-4 py-2 w-lg border border-gray-300 rounded px-4", id_ "todo-item", makeAttribute "x-data" ("{ editMode: false, titleValue: \"" <> title <> "\", editTitle: \"" <> title <> "\",setData() {this.editTitle = this.titleValue; this.editMode = false}}")] $ do
    div_ [class_ "flex items-center w-full", makeAttribute "x-show" "!editMode"] $ do
      p_ [ class_ "text-sm/6 font-semibold text-gray-900", makeAttribute "x-text" "editTitle", id_ ("todo-list-item-" <> T.pack (show todoId)) ] (toHtml title)
      button_ 
        [ class_ "ml-auto mr-4 border border-green-500 text-green-500 px-3 py-1 h-full cursor-pointer rounded", makeAttribute "@click" "editMode = true"] "Edit"
      button_ 
        [ class_ "border border-red-500 text-red-500 px-3 py-1 h-full cursor-pointer rounded"
        , makeAttribute "hx-target" "closest li", makeAttribute "hx-swap" "delete", makeAttribute "hx-delete" ("/todo/" <> T.pack (show todoId))
        ] "Delete"
    form_ [class_ "w-full flex items-center", makeAttribute "hx-swap" "none", makeAttribute "hx-patch" "/todo", makeAttribute "hx-ext" "json-enc", makeAttribute "x-show" "editMode"] $ do
        input_ [id_ "todo-input-id", class_ "hidden w-40 outline-none border-none", type_ "text", name_ "editId", value_ (T.pack (show todoId))]
        input_ [id_ "todo-input-title", class_ "w-40 outline-none border-none", placeholder_ "Enter todo", type_ "text", name_ "editTitle", makeAttribute "x-model" "titleValue"]
        button_
          [ class_ "ml-auto mr-4 border border-green-500 px-3 py-1 h-full cursor-pointer rounded"
          , makeAttribute "@click" "setData"
          ] "Save"
        button_
          [ class_ "border border-red-500 px-3 py-1 h-full cursor-pointer rounded"
          , makeAttribute "@click" "editMode = false", type_ "button"] "Cancel"

todoList :: [Todo] -> Html ()
todoList todos = do
  ul_ [role_ "list", class_ "divide-y divide-gray-200 my-6 space-y-4", id_ "todo-items"] $ do
    mapM_ (\(Todo tid title) -> todoItem tid title) todos

landingPage :: Html ()
landingPage = do
  doctype_
  html_ $ do
    head_ $ do
      title_ "Todo App"
      link_ [rel_ "stylesheet", href_ "/static/output.css"]
      with (script_ "") [src_ "/static/htmx.min.js"]
      with (script_ "") [src_ "/static/htmx-json.js"]
      with (script_ "") [src_ "https://cdnjs.cloudflare.com/ajax/libs/alpinejs/3.14.9/cdn.min.js", integrity_ "sha512-KSdieUYxSxr/laB3Bh5TP8GAng49b2qRfdcnFvh8OuPpPgksA189OQ9v1A3gIz5P9s3A4aXMe5uiHLMfla60Uw==", crossorigin_ "anonymous", defer_ "true"]
    body_ [class_ "mx-auto max-w-3xl my-4 flex flex-col items-center justify-center"] $ do
      h1_ [class_ "text-3xl font-bold mb-4"] "Todo App"
      p_ [class_ "text-base font-medium mb-6"] "List of things to do"

      form_ [class_ "w-full bg-gray-200 flex items-center w-sm h-12 rounded-lg", makeAttribute "hx-post" "/todo", makeAttribute "hx-ext" "json-enc", makeAttribute "hx-target" "#todo-items", makeAttribute "hx-swap" "afterbegin", makeAttribute "x-data" "{ title: '' }"] $ do
        input_ [id_ "todo-input", class_ "w-full h-full rounded-s-lg px-2 outline-none border-none", placeholder_ "Enter todo", type_ "text", name_ "newTitle", makeAttribute "x-model" "title"]
        button_
          [ class_ "bg-red-500 text-white px-4 h-full cursor-pointer rounded-e-lg"
          , makeAttribute "@click" "setTimeout(() => title = '', 500)"
          ] "Add"


      with (div_ mempty)
        [ id_ "todo-list"
        , makeAttribute "hx-get" "/todos"
        , makeAttribute "hx-trigger" "refresh, load"
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

createTodo :: NewTodo -> Handler (Html ())
createTodo newTodo = do
  todo <- liftIO $ createTodoQuery newTodo
  let tId = todoId todo
  let todoTitle = title todo
  pure (todoItem tId todoTitle)

updateTodo :: EditTodo -> Handler ()
updateTodo todo = do
  liftIO $ updateTodoQuery todo
  pure ()

deleteTodo :: Int -> Handler NoContent
deleteTodo todoId = do
  liftIO $ deleteTodoQuery todoId
  pure NoContent

todoAPI :: Server TodoAPI
todoAPI = (entryPoint :<|> fetchTodos :<|> createTodo :<|> updateTodo :<|> deleteTodo)

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