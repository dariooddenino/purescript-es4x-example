module Es4x.Web.Template.Handlebars where

import Prelude
import Effect (Effect)

-- move to a new file
data TemplateResult
foreign import renderTemplate :: String -> String -> (TemplateResult -> Effect Unit) -> Effect Unit
foreign import getTemplateResult :: TemplateResult -> String
