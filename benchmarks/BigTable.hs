import qualified BigTable.Blaze     as Blaze
import qualified BigTable.Lucid     as Lucid
import qualified BigTable.Nice      as Nice
import           Criterion.Main     (bgroup, defaultMain, env)
import qualified Data.Text.Lazy.IO  as T
import           System.Environment
import           Text.Read          (readMaybe)

{-# NOINLINE rows #-}
rows :: Int -> [[Int]]
rows i = replicate i [1..10]

main :: IO ()
main = do
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

    go f i = env (return (rows i)) f

  check "nice = blaze" Nice.bigTable Blaze.bigTable
  check "nice = lucid" Nice.bigTable Lucid.bigTable
  check "lucid = blaze" Lucid.bigTable Blaze.bigTable

  defaultMain
    [ bgroup (show i)
      [ go Blaze.benchmark i
      , go Nice.benchmark i
      , go Lucid.benchmark i
      ]
    | i <- [10, 100, 1000]
    ]

