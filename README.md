# TODO

Keep the api as similar to the original as possible

[ ] ReaderT to thread the immutable config (not the router!)
[ ] Database connection
[ ] Better template handling
[X] Auto response encoding
    Instead of manually calling end with the type I should return a datatype
    Then the appropriate function and headers are used
[ ] Type safety for the request if it's json
[ ] Type safety for the path
[ ] Should we be in Aff maybe?
[ ] How does es4x handle GET/POSt/etc methods?

I think ideally we could have something like

data Route = Route path [methods] reqtype restype

and then

doRoute route \req -> do
  pure someresponsedependingontype
  







# YESOD

newtype HandlerFor site a = HandlerFor { unHandlerFor :: HandlerData site site -> IO a }

data HandlerData child site = HandlerData
  { handlerRequest :: YesodRequest
  , handlerEnv :: RunHandlerEnv child site 
  , handlerState :: IORef GHState
  , handlerResource :: InternalState
  }
  
# HTTPURE

ServerM = Effect (Effect Unit -> Effect Unit)
just an Effect containing a callback to close the server

serve :: Int -> (Request -> ResponseM) -> Effect Unit -> ServerM

Middleware
(Request -> ResponseM) -> Request -> ResponseM

Router 
Request -> ResponseM

Request { method, path, query, headers, body }

ResponseM = Aff Response

Response = { headers, status, writeBody :: response -> Aff Unit }
