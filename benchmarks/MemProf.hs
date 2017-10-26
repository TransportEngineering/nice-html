{-# OPTIONS_GHC -fprof -fprof-auto-all #-}
import qualified BigTable.Nice as Nice
import qualified Weigh         as Mem

{-# NOINLINE rows #-}
rows :: Int -> [[Int]]
rows i = replicate i [1..10]

main :: IO ()
main = Mem.mainWith (Nice.weight (rows 100))

