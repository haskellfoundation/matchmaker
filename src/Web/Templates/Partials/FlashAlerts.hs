{-# LANGUAGE QuasiQuotes #-}
module Web.Templates.Partials.FlashAlerts where

import Data.String.Interpolate

infoTemplate :: Text -> Text
infoTemplate msg = [i|
  <div class="block text-sm text-left text-purple-600 bg-purple-200 border border-purple-400 h-12 flex items-center p-4 rounded-sm" role="alert">
    #{msg}
    <button type="button" data-dismiss="alert" aria-label="Close" onclick="this.parentElement.remove();">
        <span class="absolute top-0 bottom-0 right-0 text-2xl px-3 py-1 hover:text-red-900" aria-hidden="true" >×</span>
    </button>
  </div>
|]

errorTemplate :: Text -> Text
errorTemplate msg = [i|
  <div class="block text-sm text-red-600 bg-red-200 border border-red-400 h-12 flex items-center p-4 rounded-sm relative" role="alert">
    <strong class="mr-1">Error!</strong> #{msg}
    <button type="button" data-dismiss="alert" aria-label="Close" onclick="this.parentElement.remove();">
        <span class="absolute top-0 bottom-0 right-0 text-2xl px-3 py-1 hover:text-red-900" aria-hidden="true" >×</span>
    </button>
  </div>
|]
