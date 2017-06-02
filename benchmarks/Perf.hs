{-# LANGUAGE BangPatterns #-}
import qualified BigTable.Blaze      as Blaze
import qualified BigTable.Lucid      as Lucid
import qualified BigTable.Nice       as Nice
import qualified BigTable.NiceWriter as NiceWriter
import qualified Criterion.Main      as Perf
import qualified Data.Text.Lazy.IO   as T

{-# NOINLINE rows #-}
rows :: Int -> [[Int]]
rows i = replicate i [1..10]

main :: IO ()
main = do

  -- Sanity checks
  let
    check l f g =
      if f' == g'
      then putStrLn ("OK: " ++ l)
      else do
        putStrLn ("FAILED: " ++ l)
        putStrLn "\n### f:"
        T.putStrLn f'
        putStrLn "\n### g:"
        T.putStrLn g'
      where
        f' = f (rows 10)
        g' = g (rows 10)

  check "niceWriter = nice" NiceWriter.bigTable Nice.bigTable
  check "nice = blaze" Nice.bigTable Blaze.bigTable
  check "nice = lucid" Nice.bigTable Lucid.bigTable
  check "lucid = blaze" Lucid.bigTable Blaze.bigTable

  Perf.defaultMain
    [ Perf.bgroup (show i)
      [ Blaze.benchmark (rows i)
      , Nice.benchmark (rows i)
      , Lucid.benchmark (rows i)
      , NiceWriter.benchmark (rows i)
      ]
    | i <- [10, 100, 1000]
    ]

