module Main where

import Control.Monad.Reader
import Es4x.Web
import Prelude

import Control.Bind (bind, discard)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)
import Prim.RowList (class RowToList)
import Simple.JSON (class WriteForeign, class WriteForeignFields, writeJSON)


-- foreign import renderTemplate :: String -> String -> (Response -> Effect Response)
-- foreign import getTemplateResult :: TemplateResult -> String
--

-- This could become something like HandlerData
data Config = Config { port :: Int }
newtype App a = App (ReaderT Config Effect a)
derive newtype instance bindApp :: Bind App
derive newtype instance applicativeApp :: Applicative App
derive newtype instance monadAskApp :: MonadAsk Config App
derive newtype instance monadEffectApp :: MonadEffect App


-- createConfig :: Effect Config
-- createConfig = do
--   router <- createRouter
--   pure $ Config { router }

-- getRouter :: Config -> Router
-- getRouter (Config { router }) = router

-- route :: RoutePath -> (RoutingContext -> Effect Unit) -> App Unit
-- route path handler = do
--   Config { router } <- ask
--   route_ <- liftEffect $  createRoute router path
--   liftEffect $ handle route_ handler

-- trying some alternative stuff to reduce boilerplate
-- name?
-- data Foo = Foo { config :: Config, ctx :: RoutingContext, response :: Response }
-- type Handler a = ReaderT Foo Effect a

-- route' :: RoutePath -> Handler Unit -> App Unit
-- route' path handler = do
--   c@(Config { router }) <- ask
--   route_ <- liftEffect $ createRoute router path
--   liftEffect $ handle route_ $ \ctx -> do
--     resp <- response ctx
--     runReaderT handler $ Foo { config: c, ctx, response: resp }

-- This doesn't work like this, as it would be modifying the reader value.
-- Should I be using a State monad? Some kind of indexed monad? To model
-- middlewares? how do other libraries solve this?
-- Maybe I can just accumulate the changes and "solve" them afterwards
-- putHeader' :: String -> String -> Handler Response
-- putHeader' n v = do
--   Foo { response } <- ask
--   liftEffect $ putHeader response n v

-- ideal middle ground code:

-- route' "/json" $ do
--   putHeader "Content-Type" "application/json"
--   pure $ toJSON { message: "Hello, World!" } -- or Object?

-- ideal final code:
-- route' "/json" $ do
--   sendJSON { message: "Hello, World!" }


-- getConfig ::

-- runApp :: App Config -> Effect Config
-- runApp (App readerT) = do
--   config <- createConfig
--   runReaderT readerT config

-- TODO: return a way to close the server?
runHttpServer :: Config -> Router -> Effect Unit
runHttpServer config router = do
  let (Config { port }) = config
  s1 <- createHttpServer
  s2 <- handleRouter s1 router
  listen s2 port

-- routes :: App Config
-- routes = do
--     route "/" \ctx -> do
--       resp <- response ctx
--       endStr resp "Hello World!"

--     route' "/json" $ do
--       pure unit
--       -- ctx <- ask
--       -- resp <- liftEffect $ response ctx
--       -- resp' <- liftEffect $ putHeader resp "Content-Type" "application/json"
--       -- liftEffect $ endStr resp' "{\"message\": \"Hello, World!\"}"
--     ask

-- main' :: Effect Unit
-- main' = do
--   config <- runApp routes
--   liftEffect $ runHttpServer config 8888
--   log "Server running"

class Respondable a where
  respond :: Response -> a -> Aff Unit

instance stringRespondable :: Respondable String where
  respond resp s =
    liftEffect $ endStr resp s

data Template r = Template String { | r }

instance templateRespondable
         :: (RowToList r t
            , WriteForeignFields t r () r2
            )
         => Respondable (Template r) where
  respond resp (Template path vals) = do
    liftEffect $ renderTemplate (writeJSON vals) path \tres -> do
      -- TODO error handling
      resp' <- putHeader resp "Content-Type" "text/html; charset=UTF-8"
      endStr resp' (getTemplateResult tres)


-- TODO I think we should pass the response to the handler, or give some
--      implicit way to handle it
-- TODO abstract some inner common stuff from these
handler :: forall a. Respondable a => Router -> RoutePath -> (RoutingContext -> Aff a) -> Effect Unit
handler router path ihandler = do
  iroute <- createRoute router path
  handle iroute \ctx -> launchAff_ do
    resp <- liftEffect $ response ctx
    val <- ihandler ctx
    respond resp val

stringHandler :: Router -> RoutePath -> (RoutingContext -> Aff String) -> Effect Unit
stringHandler = handler

-- TODO we could "hardcode" or put in the reader the templates path and type?
templateHandler :: forall r t r2. RowToList r t => WriteForeignFields t r () r2 => Router -> RoutePath -> (RoutingContext -> Aff (Template r)) -> Effect Unit
templateHandler = handler

main :: Effect Unit
main = do
  let config = Config { port: 8888 }
  router <- createRouter

  stringHandler router "/" \req -> do
    pure "Hello Chunko"

  templateHandler router "/fortunes" \req -> do
    pure $ Template "templates/fortunes.hbs" { fortunes: [{ id: 1, message: "This template work" }] }

  -- just temp stuff
  runHttpServer config router
  -- server <- createHttpServer
  -- s <- handleRouter server router
  -- listen s 8888

  log "Server started on port 8888"
