{-# LANGUAGE BangPatterns #-}
import qualified BigTable.Blaze      as Blaze
import qualified BigTable.Lucid      as Lucid
import qualified BigTable.Nice       as Nice
import qualified BigTable.NiceWriter as NiceWriter
import           Control.Monad       (forM_)
import qualified Data.Text.Lazy.IO   as T
import qualified Weigh               as Mem

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

  Mem.mainWith $ forM_ [10, 100, 1000] $ \i -> do
    let table = rows i
    Blaze.weight table
    Nice.weight table
    NiceWriter.weight table
    Lucid.weight table

