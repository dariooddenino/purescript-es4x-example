module Main where

import Control.Monad.Reader
import Es4x.Web
import Es4x.Web.Template.Handlebars
import Prelude

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
  -- create db pools, etc.
  pure $ Config { port: 8888 }

runApp :: forall a. App a -> Effect Unit
runApp (App readerT) = launchAff_ do
  config <- createConfig
  runReaderT readerT config

runHandler :: forall a. Config -> App a -> Effect Unit
runHandler config (App readerT) = do
  launchAff_ $ runReaderT readerT config

runHttpServer :: Router -> App Unit
runHttpServer router = do
  config <- ask
  let (Config { port }) = config
  s1 <- liftEffect $ createHttpServer
  s2 <- liftEffect $ handleRouter s1 router
  liftEffect $ listen s2 port

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
      resp' <- putHeader resp "Content-Type" "text/html; charset=UTF-8"
      endStr resp' (getTemplateResult tres)
else
-- This won't work, but it's enough for now
instance jsonRespondable
        :: WriteForeign a
        => Respondable a where
  respond resp a = liftEffect $ do
    resp' <- putHeader resp "Content-Type" "application/json"
    endStr resp' $ writeJSON a


createRoute' :: Router -> RoutePath -> App Route
createRoute' router path = liftEffect $ createRoute router path

handler :: forall a. Respondable a => Router -> RoutePath -> (RoutingContext -> App a) -> App Unit
handler router path ihandler = do
  iroute <- createRoute' router path
  config <- ask
  liftEffect $ handle iroute \ctx -> do
    resp <- response ctx
    runHandler config $ do
      val <- ihandler ctx
      respond resp val

stringHandler :: Router -> RoutePath -> (RoutingContext -> App String) -> App Unit
stringHandler = handler

templateHandler :: forall r t r2. RowToList r t => WriteForeignFields t r () r2 => Router -> RoutePath -> (RoutingContext -> App (Template r)) -> App Unit
templateHandler = handler

jsonHandler :: forall a. WriteForeign a => Router -> RoutePath -> (RoutingContext -> App a) -> App Unit
jsonHandler = handler

main :: Effect Unit
main = do
  router <- createRouter
  runApp do

    stringHandler router "/" \req -> do
      pure "Hello String"

    templateHandler router "/fortunes" \req -> do
      pure $ Template "templates/fortunes.hbs" { fortunes: [{ id: 1, message: "Hello Template" }] }

    jsonHandler router "/json" \req -> do
      pure { bananas: [1, 2, 3] }

    stringHandler router "/wait" \_ -> do
      liftAff $ delay $ Milliseconds 5000.0
      Config { port } <- ask
      pure $ "Waited on port " <> show port

    runHttpServer router
