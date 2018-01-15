{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
-- | A 'Free' monad-based HTML markup monad. Unlike the writer-like monad in
-- "Text.Html.Nice.Writer", sequencing bits of markup together results in them
-- being nested, rather than concatenated.
--
-- There is no kitchen-sink of HTML5 elements provided. Use OverloadedLabels
-- instead: @ #div :: Markup n () @ and @ #div :: MakeNode n () @.
--
-- Since the monad and applicative of 'Markup' here nests rather than
-- concatenates, the function 'nodes' is provided to put a list of nodes
-- in sequence. You can use OverloadedLists for convenient syntax here.
--
-- Example Markup:
--
-- @
-- #html >>
-- [ #head >> #title >> "Title goes here"
-- , #body >>
--   [ #h1 >> "heading goes here"
--   , #p >> "i am a paragraph below the heading"
--   , do i <- branch [0..100]
--        builder (decimal i)
--   ]
-- ]
-- @
--
module Text.Html.Nice.FreeMonad
  ( -- * Markup
    Markup
  , FastMarkup
  , runMarkup
    -- * Compiling
  , compile
    -- * Rendering
    -- ** Rendering through some monad
  , renderM
  , renderMs
    -- ** Pure rendering
  , render
  , Identity (..)
    -- * Util
  , TLB.toLazyText
    -- * Special HTML elements
  , doctype
    -- ** Basic node types
  , node
  , Attr (..)
  , attr
  , empty
    -- ** Text types
  , text
  , lazyText
  , builder
  , unescape
  , string
    -- * Dynamic nodes
  , dynamic
  , hole
  , embed
    -- ** Sequential nodes
  , nodes
  , branch
    -- ** Streamed dynamic nodes
  , stream
    -- ** Combinators
  , sub
    -- * Useful exports
    -- ** Useful 'TLB.Builder' functions
  , TLB.decimal
  , TLB.realFloat
  , TLB.fromText
  , TLB.fromString
  , TLB.fromLazyText
    -- ** Text builder
  , TLB.Builder
    -- ** Void type
  , Void
  ) where
import           Control.Monad
import           Control.Monad.Free.Church
import           Data.Bifunctor
import           Data.Default.Class
import           Data.Foldable                    as F
import qualified Data.Functor.Foldable            as F
import           Data.Functor.Identity
#if __GLASGOW_HASKELL__ >= 802
import           Data.Proxy                       (Proxy (..))
#endif
import           Data.String                      (IsString (..))
import           Data.Text                        (Text)
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Builder           as TLB
import qualified Data.Text.Lazy.Builder.Int       as TLB
import qualified Data.Text.Lazy.Builder.RealFloat as TLB
import qualified Data.Vector                      as V
import           Data.Void
import           GHC.Exts                         (IsList (..))
import           GHC.OverloadedLabels             (IsLabel (..))
#if __GLASGOW_HASKELL__ >= 802
import           GHC.TypeLits                     (KnownSymbol, symbolVal)
#else
import           GHC.TypeLits                     (KnownSymbol, symbolVal')
#endif
import           Text.Html.Nice.Internal

-- | 'Markup' is a free monad based on the base functor to 'Markup\'F'
--
-- Beware: this is a wacky monad. '>>' does *not* sequence nodes together;
-- instead, it nests them. To lay out nodes sequentially, use 'nodes'.
--
-- = Syntactic sugar
-- 'Markup' supports @OverloadedLabels@, @OverloadedStrings@ and
-- @OverloadedLists@.
--
-- == @OverloadedStrings@
-- @("foo" :: Markup n a) = 'text' "foo"@
--
-- == @OverloadedLists@
-- @([a,b,c] :: Markup n a) = 'nodes' [a,b,c]@
--
-- == @OverloadedLabels@
-- The 'IsLabel' instances give a convenient way to write nodes.
--
-- === Nodes without attributes
-- @ (#foo :: 'Markup' n a) = 'node' "foo" [] @
--
-- === Nodes with attributes
-- @ 'attr' #foo [a,b,c] = 'node' "foo" [a,b,c] @
--
newtype Markup n a = Markup { unMarkup :: F (Markup'F n) a }
  deriving (Functor, Applicative, Monad, MonadFree (Markup'F n))

instance Default (Markup n a) where
  def = empty

instance Monoid (Markup n a) where
  mempty = empty
  mappend a b = nodes [a, b]

instance IsString (Markup n a) where
  fromString = text . fromString

instance IsList (Markup n a) where
  type Item (Markup n a) = Markup n a
  fromList = nodes
  toList _ = error "haha, fooled you, Markup has no toList"

instance (a ~ (), KnownSymbol s) => IsLabel s (Markup n a) where
#if __GLASGOW_HASKELL__ >= 802
  fromLabel = node (fromString (symbolVal (Proxy :: Proxy s))) []
#else
  fromLabel p = node (fromString (symbolVal' p)) []
#endif

newtype MakeNode n a = N ([Attr n] -> Markup n a)

instance (a ~ (), KnownSymbol s) => IsLabel s (MakeNode n a) where
#if __GLASGOW_HASKELL__ >= 802
  fromLabel = N (node (fromString (symbolVal (Proxy :: Proxy s))))
#else
  fromLabel p = N (node (fromString (symbolVal' p)))
#endif

instance Bifunctor Markup where
  first f = Markup . hoistF (first f) . unMarkup
  second = fmap

-- | For use with @OverloadedLabels@.
--
-- @ 'attr' #x [a,b,c] = 'node' "x" [a,b,c] @
--
attr :: MakeNode n a -> [Attr n] -> Markup n a
attr (N f) = f

runMarkup :: Markup n a -> Markup' n
runMarkup h = runF (unMarkup h) (const Empty) F.embed

-- | Compile a 'Html' for use with 'render' and its friends.
--
-- See also: 'compile_'.
compile :: Markup n a -> FastMarkup n
compile = compile_ . runMarkup

-- | Make a node with some attributes.
node :: Text -> [Attr n] -> Markup n ()
node t v = liftF (NodeF t (V.fromList v) ())

-- | Insert text and escape it
text :: Text -> Markup n a
text t = liftF (TextF DoEscape (StrictT t))

-- | Insert text and escape it
lazyText :: TL.Text -> Markup n a
lazyText t = liftF (TextF DoEscape (LazyT t))

-- | Insert text and escape it
builder :: TLB.Builder -> Markup n a
builder t = liftF (TextF DoEscape (BuilderT t))

-- | Insert text and escape it
string :: String -> Markup n a
string t = liftF (TextF DoEscape (BuilderT (TLB.fromString t)))

-- | Insert text and don't escape it
unescape :: Text -> Markup n a
unescape t = liftF (TextF Don'tEscape (StrictT t))

-- | Insert a dynamic value.
dynamic :: n -> Markup n a
dynamic n = liftF (HoleF DoEscape n)

hole :: Markup (a -> a) t
hole = dynamic id

-- | For each element of a list of branches, generate sequential markup
branch :: [a] -> Markup n a
branch = liftF . ListF

-- | For each element of a list, generate sequential markup
nodes :: [Markup n a] -> Markup n a
nodes = join . liftF . ListF

-- | Empty node. Terminates 'Markup' to this point
empty :: Markup n a
empty = liftF EmptyF

-- | Insert a sub-template.
sub :: Markup n a -> Markup (FastMarkup n) a
sub x = liftF (HoleF Don'tEscape (compile x))

-- | Insert a sub-template.
{-# INLINE embed #-}
embed :: (t -> FastMarkup n) -> Markup (t -> FastMarkup n) a
embed = dynamic

{-# INLINE stream #-}
stream :: Foldable f
       => Markup (a -> n) r'
       -> Markup (f a -> FastMarkup n) r
stream m = embed $ \fa -> case F.toList fa of
  []   -> FEmpty
  list -> FStream (ListS list fm)
    where
      !fm = compile m

doctype :: Markup n a
doctype = liftF DoctypeF

