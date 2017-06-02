{-# LANGUAGE OverloadedStrings #-}
module BigTable.Blaze where
import           Criterion.Main                (Benchmark, bench, nf)
import           Data.Text.Lazy                (Text)
import           Text.Blaze.Html.Renderer.Text as H
import           Text.Blaze.Html5              as H
import           Weigh                         (Weigh, func)

-- | Render the argument matrix as an HTML table.
--
bigTable :: [[Int]]  -- ^ Matrix.
         -> Text     -- ^ Result.
bigTable t = renderHtml $ do
  h1 "i am a real big old table\n"
  p "i am good at lots of static data\n"
  p "i am glab at lots of static data\n"
  p "i am glob at lots of static data\n"
  p "i am glib at lots of static data\n"
  p "i am glub at lots of static data\n"
  p "i am glom at lots of static data\n"
  p "i am glof at lots of static data\n"
  p "i am gref at lots of static data\n"
  p "i am greg at lots of static data\n"
  table $ do
    thead (tr (mapM_ (th . toHtml) [1..10 :: Int]))
    tbody (mapM_ row t)
  p "i am good at lots of static data\n"
  p "i am glab at lots of static data\n"
  p "i am glob at lots of static data\n"
  p "i am glib at lots of static data\n"
  p "i am glub at lots of static data\n"
  p "i am glom at lots of static data\n"
  p "i am glof at lots of static data\n"
  p "i am gref at lots of static data\n"
  p "i am greg at lots of static data\n"
  where
    row r = tr (mapM_ (\t -> do
                          p "hi!\n"
                          td (toHtml t)
                          p "hello!\n") r)

benchmark :: [[Int]] -> Benchmark
benchmark rows = bench "blaze" (bigTable `nf` rows)

weight :: [[Int]] -> Weigh ()
weight i = func (show (length i) ++ "/blaze") bigTable i
