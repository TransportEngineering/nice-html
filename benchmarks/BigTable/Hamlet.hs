{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module BigTable.Hamlet where
import           Criterion                     (Benchmark, bench, nf)
import           Data.Text.Lazy                (Text)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet
import           Weigh

bigTable :: [[Int]] -> Text
bigTable rows = renderHtml $
  [hamlet|
  <h1>i am a real big old table
  <p>i am good at lots of static data
  <p>i am glab at lots of static data
  <p>i am glob at lots of static data
  <p>i am glib at lots of static data
  <p>i am glub at lots of static data
  <p>i am glom at lots of static data
  <p>i am glof at lots of static data
  <p>i am gref at lots of static data
  <p>i am greg at lots of static data
  <table>
    <thead>
      <tr>
        $forall i <- header
          <th>#{i}
    <tbody>
      $forall row <- rows
        <tr>
          $forall col <- row
            <td>
              <p>hi!
              #{col}
              <p>hello!
  <p>i am good at lots of static data
  <p>i am glab at lots of static data
  <p>i am glob at lots of static data
  <p>i am glib at lots of static data
  <p>i am glub at lots of static data
  <p>i am glom at lots of static data
  <p>i am glof at lots of static data
  <p>i am gref at lots of static data
  <p>i am greg at lots of static data|]
  ()
  where
    header = [1..10 :: Int]

benchmark :: [[Int]] -> Benchmark
benchmark rows = bench "hamlet" (bigTable `nf` rows)

weight :: [[Int]] -> Weigh ()
weight i = func (show (length i) ++ "/hamlet") bigTable i
