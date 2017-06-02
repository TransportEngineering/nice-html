{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module BigTable.Nice where
import           Criterion.Main         (Benchmark, bench, nf)
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Text.Lazy         (Text)
import           Data.Text.Lazy.Builder (fromString)
import           Text.Html.Nice         ((:$) (..), Render (..))
import           Text.Html.Nice.Monad
import           Weigh                  (Weigh, func)

rows :: FastMarkup ([[Int]] -> FastMarkup (FastMarkup Builder))
rows = compile
  [ #h1 >> "i am a real big old table\n"
  , #p >> "i am good at lots of static data\n"
  , #p >> "i am glab at lots of static data\n"
  , #p >> "i am glob at lots of static data\n"
  , #p >> "i am glib at lots of static data\n"
  , #p >> "i am glub at lots of static data\n"
  , #p >> "i am glom at lots of static data\n"
  , #p >> "i am glof at lots of static data\n"
  , #p >> "i am gref at lots of static data\n"
  , #p >> "i am greg at lots of static data\n"
  , #table >>
    [ #thead >> #tr >> do
        i <- branch [1..10 :: Int]
        #th >> builder (decimal i)
    , #tbody >> stream (#tr >> stream
      [ #p >> "hi!\n"
      , #td >> dynamic decimal
      , #p >> "hello!\n"
      ])
    ]
  , #p >> "i am good at lots of static data\n"
  , #p >> "i am glab at lots of static data\n"
  , #p >> "i am glob at lots of static data\n"
  , #p >> "i am glib at lots of static data\n"
  , #p >> "i am glub at lots of static data\n"
  , #p >> "i am glom at lots of static data\n"
  , #p >> "i am glof at lots of static data\n"
  , #p >> "i am gref at lots of static data\n"
  , #p >> "i am greg at lots of static data\n"
  ]

bigTable :: [[Int]] -> Text
bigTable table = toLazyText (runIdentity (r (rows :$ table)))

benchmark :: [[Int]] -> Benchmark
benchmark t = bench "nice" (bigTable `nf` t)

weight :: [[Int]] -> Weigh ()
weight i = func (show (length i) ++ "/nice") bigTable i
