module Main where

import Control.Bind (bind, discard)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
-- import Es4x.Web (createRouter, end, endStr, handlePath, putHeader, response, runHttpServer, setChunked, write)
import Es4x.Web


-- foreign import renderTemplate :: String -> String -> (Response -> Effect Response)
-- foreign import getTemplateResult :: TemplateResult -> String
--

main :: Effect Unit
main = do
  router <- createRouter

  handlePath router "/fortunes" \ctx -> do
    -- ezRender ctx
    renderTemplate "{\"fortunes\":[]}" "templates/fortunes.hbs" \tres -> do
      resp <- response ctx
      resp' <- putHeader resp "Content-Type" "text/html; charset=UTF-8"
      endStr resp' (getTemplateResult tres)

  
  handlePath router "/plaintext" \ctx -> do
    resp <- response ctx
    resp' <- putHeader resp "Content-Type" "text/plain"
    endStr resp' "Hello, World!"

  handlePath router "/json" \ctx -> do
    resp <- response ctx
    resp' <- putHeader resp "Content-Type" "application/json"
    endStr resp' "{\"message\": \"Hello, World\"}"

  handlePath router "/" \req -> do
    resp <- response req
    endStr resp  "Hello Purescript ES4X!"

  handlePath router "/page" \req -> do
    resp <- response req
    setChunked resp
    write resp "You are on a page"
    end resp

  runHttpServer router 8888

  log "Server started on port 8888"
