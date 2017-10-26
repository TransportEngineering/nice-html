{-# LANGUAGE OverloadedStrings #-}
module TodoList where
import           Data.Text                   (Text)
import           Text.Html.Nice              ((:$) (..), Attr (..), Builder,
                                              FastMarkup, Render (..))
import           Text.Html.Nice.Writer
import           Text.Html.Nice.Writer.Html5

data Todo = Todo
  { todoDate :: Text
  , todoText :: Text
  }

todos :: [Todo]
todos =
  [ Todo "october 25 2017" "write todo list <html>asdf</html>"
  , Todo "october 26 2017" "write another todo list"
  ]

template :: FastMarkup ([Todo] -> FastMarkup Text)
template = compile $ do
  doctype_
  html_ $ do
    head_ $ title_ "Todo list"
    body_ $ do
      h1_ "Todo list"
      stream $ div_ ! "class" := "todo-item" $ do
        text "\n<script></script>\n"
        b_ (dynamic todoText)
        " ("
        dynamic todoDate
        ")"

test :: IO Builder
test = r (template :$ todos)
