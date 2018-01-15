{-# LANGUAGE OverloadedStrings #-}
module BigTable.TypeOfHtml where
import           Criterion.Main (Benchmark, bench, nf)
import           Data.Text.Lazy (Text)
import           Html
import           Weigh          (Weigh, func)

-- | Render the argument matrix as an HTML table.
--
bigTable :: [[Int]]  -- ^ Matrix.
         -> Text     -- ^ Result.
bigTable rows = renderText $
  h1_ ("i am a real big old table\n" :: Text)
  # p_ ("i am good at lots of static data\n" :: Text)
  # p_ ("i am glab at lots of static data\n" :: Text)
  # p_ ("i am glob at lots of static data\n" :: Text)
  # p_ ("i am glib at lots of static data\n" :: Text)
  # p_ ("i am glub at lots of static data\n" :: Text)
  # p_ ("i am glom at lots of static data\n" :: Text)
  # p_ ("i am glof at lots of static data\n" :: Text)
  # p_ ("i am gref at lots of static data\n" :: Text)
  # p_ ("i am greg at lots of static data\n" :: Text)
  # table_ (
    thead_ (tr_ (map (th_ . show) [1..10 :: Int]))
    # tbody_ (map row rows))
  # p_ ("i am good at lots of static data\n" :: Text)
  # p_ ("i am glab at lots of static data\n" :: Text)
  # p_ ("i am glob at lots of static data\n" :: Text)
  # p_ ("i am glib at lots of static data\n" :: Text)
  # p_ ("i am glub at lots of static data\n" :: Text)
  # p_ ("i am glom at lots of static data\n" :: Text)
  # p_ ("i am glof at lots of static data\n" :: Text)
  # p_ ("i am gref at lots of static data\n" :: Text)
  # p_ ("i am greg at lots of static data\n" :: Text)
  where
    row r = tr_ (map
                 (\t -> td_ $
                   p_ ("hi!\n" :: Text)
                   # show t
                   # p_ ("hello!\n" :: Text))
                 r)

benchmark :: [[Int]] -> Benchmark
benchmark rows = bench "type-of-html" (bigTable `nf` rows)

weight :: [[Int]] -> Weigh ()
weight i = func (show (length i) ++ "/type-of-html") bigTable i
