module Web.Templates.Helpers where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Relude.Unsafe as U

import Web.Templates.Types

-- | Use this function in a View module so that the template name and location
-- can be inferred from the name of the view
moduleName :: Q (TExp ModuleName)
moduleName = do
    name <- loc_module <$> qLocation
    [|| ModuleName $ U.last $ T.splitOn "." $ toText @String name ||]

emptyAssigns :: TemplateAssigns
emptyAssigns = TemplateAssigns HM.empty
