module Main where

import Control.Monad.Reader
import Es4x.Web
import Es4x.Web.Template.Handlebars
import Prelude

import Control.Bind (bind, discard)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)
import Prim.RowList (class RowToList)
import Simple.JSON (class WriteForeign, class WriteForeignFields, writeJSON)
import Types (App(..), Config(..))





createConfig :: Aff Config
createConfig = do
  pure $ Config { port: 8888 }

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


runApp :: forall a. App a -> Aff a
runApp (App readerT) = do
  config <- createConfig
  runReaderT readerT config

runHandler :: forall a. Config -> App a -> Effect Unit
runHandler config (App handler) = do
  launchAff_ $ runReaderT handler config

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

data Template r = Template String { | r }

class Respondable a where
  respond :: Response -> a -> App Unit

instance stringRespondable :: Respondable String where
  respond resp s =
    liftEffect $ endStr resp s

else
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
else
-- TODO: this right now works as a catch all, I think. I think it becomes a problem once more instances are being created?
-- I think we have to use Foreign or wrap in another data type. This also changes how handler should be written.
instance jsonRespondable
        :: WriteForeign a
        => Respondable a where
  respond resp a = liftEffect $ do
    resp' <- putHeader resp "Content-Type" "application/json"
    endStr resp' $ writeJSON a


-- data Foo = Foo Int
-- instance fooRespondable :: Respondable foo where
--   respond resp a = liftEffect $ endStr resp "Foo"

createRoute' :: Router -> RoutePath -> App Route
createRoute' router path = liftEffect $ createRoute router path

-- TODO I think we should pass the response to the handler, or give some
--      implicit way to handle it
handler :: forall a. Respondable a => Router -> RoutePath -> (RoutingContext -> App a) -> App Unit
handler router path ihandler = do
  iroute <- createRoute' router path
  -- iroute <- lift $ liftEffect $ createRoute router path
  config <- ask
  liftEffect $ handle iroute \ctx -> do
    resp <- response ctx
    runHandler config $ do
      val <- ihandler ctx
      respond resp val

stringHandler :: Router -> RoutePath -> (RoutingContext -> App String) -> App Unit
stringHandler = handler

-- TODO we could "hardcode" or put in the reader the templates path and type?
templateHandler :: forall r t r2. RowToList r t => WriteForeignFields t r () r2 => Router -> RoutePath -> (RoutingContext -> App (Template r)) -> App Unit
templateHandler = handler

jsonHandler :: forall a. WriteForeign a => Router -> RoutePath -> (RoutingContext -> App a) -> App Unit
jsonHandler = handler

main :: Effect Unit
main = do
  router <- createRouter
  launchAff_ $ runApp do

    stringHandler router "/" \req -> do
      pure "Hello String"

    templateHandler router "/fortunes" \req -> do
      pure $ Template "templates/fortunes.hbs" { fortunes: [{ id: 1, message: "This template work" }] }

    jsonHandler router "/json" \req -> do
      pure { bananas: [1, 2, 3] }

    stringHandler router "/wait" \_ -> do
      liftAff $ delay $ Milliseconds 5000.0
      Config { port } <- ask
      pure $ "Waited on port " <> show port

    config <- ask
    liftEffect $ runHttpServer config router


  -- let config = Config { port: 8888 }
  -- router <- createRouter

  -- stringHandler router "/" \req -> do
  --   pure "Hello String"

  -- templateHandler router "/fortunes" \req -> do
  --   pure $ Template "templates/fortunes.hbs" { fortunes: [{ id: 1, message: "This template work" }] }

  -- jsonHandler router "/json" \req -> do
  --   pure { bananas: [1, 2, 3] }

  -- -- just temp stuff
  -- runHttpServer config router

  -- log "Server started on port 8888"
