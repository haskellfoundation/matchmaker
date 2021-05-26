-- | This module is a convenience module, it re-exports the most common modules
-- required to write a handler:
--  - 'DB' exports the @HasDB@ class, which allows you to use 'runDB'
--  - 'Foundation' exports the actual @Handler@ type as well as the @Foundation@ type
--  - 'Yesod.Core' provides the majority of the yesod functionality you might require
--  i.e. the @Html@ ContentType and 'redirect'
--
--  A word of caution: It behooves us to keep the re-exports here to a minimum. Choke points
--  like this in the module graph can really explode compilation times, and the more modules
--  that get added here, the harder it becomes to manage module cycles.
module ImportYesod
  (
    module DB,
    module Foundation,
    module Yesod.Core,
  ) where

import DB
import Foundation
import Yesod.Core
