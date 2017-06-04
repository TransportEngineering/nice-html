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
    -- * Re-exports of 'TLB.Builder' functions
  , module Data.Text.Lazy.Builder
  , module Data.Text.Lazy.Builder.Int
  , module Data.Text.Lazy.Builder.RealFloat
    -- * Re-exports of lazy 'Text' functions
  , Text
  , fromStrict
  , toStrict
  ) where
import           Data.Text.Lazy                   (Text, fromStrict, toStrict)
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int
import           Data.Text.Lazy.Builder.RealFloat
import           Text.Html.Nice.Internal          ((:$) (..), Attr (..),
                                                   FastMarkup, Render (..),
                                                   recompile, render, renderM,
                                                   renderMs)
import           Text.Html.Nice.Writer
import           Text.Html.Nice.Writer.Html5
