import qualified BigTable.Blaze as Blaze
import qualified BigTable.Lucid as Lucid
import qualified BigTable.Nice  as Nice
import           Criterion.Main (defaultMain)
import qualified Data.Text.Lazy.IO as T

{-# NOINLINE rows #-}
rows :: [[Int]]
rows = replicate 5 [1..10]

main :: IO ()
main = do
  let check l f g =
        if f' == g'
        then putStrLn ("OK: " ++ l)
        else do
          putStrLn ("FAILED: " ++ l)
          putStrLn "\n### f:"
          T.putStrLn f'
          putStrLn "\n### g:"
          T.putStrLn g'
        where f' = f rows
              g' = g rows

  check "nice = blaze" Nice.bigTable Blaze.bigTable
  check "nice = lucid" Nice.bigTable Lucid.bigTable
  check "lucid = blaze" Lucid.bigTable Blaze.bigTable

  defaultMain
    [ Blaze.benchmark rows
    , Nice.benchmark rows
    , Lucid.benchmark rows
    ]

