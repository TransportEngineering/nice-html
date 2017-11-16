module Text.Html.Nice
  (
    -- * Nice HTML writer monad
    module Text.Html.Nice.Writer
  , Attr (..)
    -- * HTML5 support
  , module Text.Html.Nice.Writer.Html5
    -- * Rendering
  , FastMarkup
  , Render (..)
  , (:$) (..)
  , renderM
  , renderMs
  , render
    -- * Utility
  , recompile
  , unlayer
  ) where
import           Text.Html.Nice.Internal     ((:$) (..), Attr (..), FastMarkup,
                                              Render (..), recompile, render,
                                              renderM, renderMs, unlayer)
import           Text.Html.Nice.Writer
import           Text.Html.Nice.Writer.Html5
