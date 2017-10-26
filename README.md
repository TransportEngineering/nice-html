# A nice HTML templating library

This is a library for HTML templating in the same vein as `blaze-html`, `lucid`,
or `type-of-html`, which all provide HTML EDSLs for Haskell.

## Overview
WIP. Check out `Text.Html.Nice.Writer` for the simplest, most recommendedest,
monadic interface.

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
module TodoList where
import           Data.Text                   (Text)
import           Text.Html.Nice              ((:$) (..), Attr (..), Builder,
                                              FastMarkup, Render (..))
import           Text.Html.Nice.Writer
import           Text.Html.Nice.Writer.Html5

data Todo = Todo
  { todoDate :: Text
  , todoText :: Text
  }

todos :: [Todo]
todos =
  [ Todo "october 25 2017" "write todo list <html>asdf</html>" -- escaped
  , Todo "october 26 2017" "write another todo list"
  ]

template :: FastMarkup ([Todo] -> FastMarkup Text)
template = compile $ do
  doctype_
  html_ $ do
    head_ $ title_ "Todo list"
    body_ $ do
      h1_ "Todo list"
      stream $ div_ ! "class" := "todo-item" $ do
        text "\n<script></script>\n" -- this gets escaped
        b_ (dynamic todoText)
        " ("
        dynamic todoDate
        ")"

test :: Monad m => m Builder
test = r (template :$ todos)
```

## Comparison

1. Unlike `blaze-html` and `lucid`: `nice-html` has a distinct template
   compilation phase, with a different type for compiled markup.

2. Unlike `type-of-html`: this compilation is done explicitly at runtime. This
   increases runtime, but enables more complex transformations. Namely
   `nice-html` can compile escaped text. `nice-html` also makes no attempt to
   ensure correct HTML.

3. Unlike each of them, `nice-html` parameterises its templating type with the
   type for the data you use in the template. This enables `nice-html` to compile
   the static parts of the dynamic parts of a template instead of just the 
   top-level stuff.
   
4. Like `lucid`, `nice-html` has a valid `Monad` interface (actually, two).

5. Unlike `lucid`, `nice-html` does not have a monad transformer. The only state
   that `nice-html` can currently keep track of is an increasing `Int` counter,
   intended to be used to keep track of the `id` of elements for use with
   JavaScript. But even this doesn't really work, because it doesn't support
   repeated elements (e.g. produced by `stream`).
   
   This makes it harder to write templates for `nice-html`, because you 
   generally can't/shouldn't use the familiar `mapM_` etc. for dynamic data as 
   you might if you were using `lucid` or `blaze`.
   

## Benchmark results (as of 0.3.0)

### Memory (includes memory overhead compilation... I think)

```
nice-html-0.3.0: benchmarks
Running 2 benchmarks...
Benchmark mem: RUNNING...
OK: nice = blaze
OK: nice = lucid
OK: lucid = blaze

Case         Allocated  GCs
10/blaze       562,152    1
10/nice        297,544    0
10/lucid       230,304    0
100/blaze    4,520,016    8
100/nice     2,952,184    5
100/lucid    1,855,256    3
1000/blaze  44,096,616   85
1000/nice   29,500,096   57
1000/lucid  18,071,008   32
Benchmark mem: FINISH
```

### Runtime

```
Benchmark perf: RUNNING...
benchmarking 10/blaze
time                 80.13 μs   (79.59 μs .. 80.73 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 80.41 μs   (80.08 μs .. 80.83 μs)
std dev              1.166 μs   (951.2 ns .. 1.531 μs)

benchmarking 10/nice
time                 34.09 μs   (33.88 μs .. 34.28 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 34.08 μs   (33.93 μs .. 34.25 μs)
std dev              542.8 ns   (477.2 ns .. 619.9 ns)
variance introduced by outliers: 12% (moderately inflated)

benchmarking 10/lucid
time                 57.40 μs   (57.03 μs .. 57.75 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 57.44 μs   (57.15 μs .. 57.67 μs)
std dev              856.7 ns   (763.9 ns .. 960.4 ns)

benchmarking 100/blaze
time                 660.8 μs   (657.2 μs .. 664.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 661.6 μs   (659.1 μs .. 664.3 μs)
std dev              8.748 μs   (7.441 μs .. 10.24 μs)

benchmarking 100/nice
time                 336.9 μs   (334.7 μs .. 338.9 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 334.3 μs   (333.2 μs .. 336.1 μs)
std dev              4.685 μs   (3.229 μs .. 7.873 μs)

benchmarking 100/lucid
time                 514.4 μs   (509.5 μs .. 519.7 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 507.1 μs   (504.6 μs .. 510.4 μs)
std dev              9.897 μs   (7.573 μs .. 14.83 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking 1000/blaze
time                 6.355 ms   (6.324 ms .. 6.380 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.424 ms   (6.400 ms .. 6.453 ms)
std dev              79.56 μs   (62.81 μs .. 106.2 μs)

benchmarking 1000/nice
time                 3.356 ms   (3.348 ms .. 3.366 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.376 ms   (3.369 ms .. 3.385 ms)
std dev              24.95 μs   (20.64 μs .. 29.55 μs)

benchmarking 1000/lucid
time                 4.885 ms   (4.850 ms .. 4.921 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.868 ms   (4.857 ms .. 4.881 ms)
std dev              36.95 μs   (31.34 μs .. 43.37 μs)

Benchmark perf: FINISH
```
