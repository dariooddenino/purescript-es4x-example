# TODO

Keep the api as similar to the original as possible

[ ] Can we get access to the response object somehow inside our handlers?
[ ] Change inner names for types
[X] ReaderT to thread the immutable config (not the router!)
[ ] How does es4x handle GET/POSt/etc methods?
[ ] data Route = Route path [methods] reqtype restype
[ ] typecheck request and response from route type
    Link to the point below
[ ] Type safety for the path
    Use a heterogeneous list here?
[ ] Database connection
[ ] Prevent overlapping routes?
[ ] Error handling like yesod
[ ] Logging
[ ] Auth
[ ] dom patching
[ ] link prefetch
[ ] styling setup
[ ] templates type safety


[X] Better template handling
[X] Auto response encoding
    Instead of manually calling end with the type I should return a datatype
    Then the appropriate function and headers are used
[X] Should we be in Aff maybe?


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
