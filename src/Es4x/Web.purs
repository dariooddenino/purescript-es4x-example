module Es4x.Web where

import Control.Bind (bind)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Types (App(..))

data Server
data RoutingContext
data Request
data Response
type Port = Int

data Router
data Route
type RoutePath = String

foreign import createHttpServer :: Effect Server
foreign import handleRequests :: Server -> (RoutingContext -> Effect Unit) -> Effect Server

foreign import createRouter :: Effect Router
foreign import createRoute :: Router -> RoutePath -> Effect Route
foreign import handle :: Route -> (RoutingContext -> Effect Unit) -> Effect Unit
foreign import handleRouter :: Server -> Router -> Effect Server


foreign import request :: RoutingContext -> Effect Request
foreign import getParam :: Request -> String -> Effect String
foreign import response :: RoutingContext -> Effect Response
foreign import putHeader :: Response -> String -> String -> Effect Response
-- | Write a chunk to the response
-- | It is REQUIRED to have called `setChunked` beforehand
foreign import write :: Response -> String -> Effect Unit
foreign import setChunked :: Response -> Effect Unit
foreign import next :: RoutingContext -> Effect Unit
foreign import endStr :: Response -> String -> Effect Unit
foreign import end :: Response -> Effect Unit
foreign import listen :: Server -> Port -> Effect Unit
