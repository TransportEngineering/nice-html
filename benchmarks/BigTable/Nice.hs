{-# LANGUAGE OverloadedStrings #-}
module BigTable.Nice where
import           Criterion.Main        (Benchmark, bench, nf)
import           Data.Functor.Identity
import           Text.Html.Nice
import           Weigh                 (Weigh, func)

rows :: FastMarkup ([[Int]] -> FastMarkup (FastMarkup Builder))
rows = compile $ do
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
    thead_ . tr_ . mapM_ (th_ . builder . decimal) $ [1..10 :: Int]
    tbody_ . stream . tr_ . stream $ do
      p_ "hi!\n"
      td_ (dynamic decimal)
      p_ "hello!\n"
  p_ "i am good at lots of static data\n"
  p_ "i am glab at lots of static data\n"
  p_ "i am glob at lots of static data\n"
  p_ "i am glib at lots of static data\n"
  p_ "i am glub at lots of static data\n"
  p_ "i am glom at lots of static data\n"
  p_ "i am glof at lots of static data\n"
  p_ "i am gref at lots of static data\n"
  p_ "i am greg at lots of static data\n"

bigTable :: [[Int]] -> Text
bigTable table =
  toLazyText (runIdentity (r (rows :$ table)))

benchmark :: [[Int]] -> Benchmark
benchmark t = bench "nice" (bigTable `nf` t)

weight :: [[Int]] -> Weigh ()
weight i = func (show (length i) ++ "/nice") bigTable i

