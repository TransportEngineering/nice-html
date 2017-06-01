{-# LANGUAGE OverloadedStrings #-}
module BigTable.Lucid where
import           Criterion.Main
import           Data.Text.Lazy                (Text)
import           Lucid.Html5
import Lucid.Base

-- | Render the argument matrix as an HTML table.
--
bigTable :: [[Int]]  -- ^ Matrix.
         -> Text     -- ^ Result.
bigTable t = renderText $ do
  h1_ "i am a real big old table\n"

  p_ "i am good at lots of static data\n"
  p_ "i am glab at lots of static data\n"
  p_ "i am glob at lots of static data\n"
  p_ "i am glib at lots of static data\n"
  p_ "i am glub at lots of static data\n"
  p_ "i am glom at lots of static data\n"
  p_ "i am glof at lots of static data\n"
  p_ "i am gref at lots of static data\n"
  p_ "i am greg at lots of static data\n"

  table_ $ do
    thead_ (tr_ (mapM_ (th_ . toHtml . show) [1..10 :: Int]))
    tbody_ (mapM_ row t)

  p_ "i am good at lots of static data\n"
  p_ "i am glab at lots of static data\n"
  p_ "i am glob at lots of static data\n"
  p_ "i am glib at lots of static data\n"
  p_ "i am glub at lots of static data\n"
  p_ "i am glom at lots of static data\n"
  p_ "i am glof at lots of static data\n"
  p_ "i am gref at lots of static data\n"
  p_ "i am greg at lots of static data\n"

  where
    row :: [Int] -> Html ()
    row r = tr_ (mapM_ (\t -> do
                          p_ "hi!\n"
                          td_ (toHtml (show t))
                          p_ "hello!\n") r)

benchmark :: [[Int]] -> Benchmark
benchmark rows = bench "lucid" (bigTable `nf` rows)

