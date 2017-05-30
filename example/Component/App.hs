{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
module Component.App where
import           Control.Lens
import           Control.Monad.Trans.State.Strict
import           Data.Default.Class
import           Data.Foldable
import           Data.Monoid
import           Data.Text                        (Text)
import           Data.Text.Lazy.Builder           (Builder)
import           Data.Void
import           GHC.Generics
import           Text.Html.Nice.Monad

data App a = App
  { appTitle     :: Markup a Void
  , appChildren  :: Markup a Void
  , appCopyright :: Maybe (Markup a Void)
  } deriving (Generic, Default)

appTemplate :: FastMarkup (App a -> FastMarkup a)
appTemplate = compile
  [ doctype
  , #html >>
    [ #head >>
      #title >>
      embed appTitle
    , #body >>
      #div `attr` ["class" := "app"] >>
      [ #section `attr` ["class" := "main"] >> embed appChildren
      , embed (fold . appCopyright)
      ]
    ]
  ]

app :: App a -> FastMarkup (FastMarkup a)
app a = fmap ($ a) appTemplate

test = app def
  { appTitle = "hello"
  , appChildren = "what a dream" }

data Button a = Button
  { buttonLabel   :: Markup a Void
  , buttonOnClick :: Text
  }

button :: FastMarkup (Button a -> FastMarkup a)
button = compile
  [ #button `attr` ["class" := "btn"] >>
    embed buttonLabel
  ]



