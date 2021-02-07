module Types where

import Prelude
import Control.Monad.Reader (class MonadAsk, ReaderT(..))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)

data Config = Config { port :: Int }
newtype App a = App (ReaderT Config Aff a)
derive newtype instance bindApp :: Bind App
derive newtype instance applicativeApp :: Applicative App
derive newtype instance monadAskApp :: MonadAsk Config App
derive newtype instance monadEffectApp :: MonadEffect App
