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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
module Text.Html.Nice.Monad
  ( -- * Markup
    Markup
  , runMarkup
  , compile
  , render
  , renderM
  , renderMs
    -- * Special HTML elements
  , doctype
    -- ** Basic node types
  , node
  , text
  , unescape
  , dynamic
  , empty
    -- ** Combinators
  , nodes
  , branch
  , sub
    -- * Useful 'TLB.Builder' functions
  , TLB.decimal
  , TLB.realFloat
  , TLB.fromText
  , TLB.fromString
  , TLB.fromLazyText
  ) where
import           Control.Monad
import           Control.Monad.Free.Church
import           Control.Monad.Trans.State.Strict
import           Data.Bifunctor
import           Data.Functor.Foldable
import           Data.String                      (IsString (..))
import           Data.Text                        (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder           as TLB
import qualified Data.Text.Lazy.Builder.Int       as TLB
import qualified Data.Text.Lazy.Builder.RealFloat as TLB
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as V
import           Data.Void
import           GHC.Exts                         (Constraint, IsList (..))
import           GHC.OverloadedLabels             (IsLabel (..))
import           GHC.TypeLits                     (KnownSymbol, symbolVal')
import           Text.Html.Nice

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

instance IsString (Markup n a) where
  fromString = text . fromString

instance IsList (Markup n a) where
  type Item (Markup n a) = Markup n a
  fromList = nodes
  toList _ = error "haha, fooled you, Markup has no toList"

instance (a ~ (), KnownSymbol s) => IsLabel s (Markup n a) where
  fromLabel p = node (fromString (symbolVal' p)) []

newtype MakeNode n a = N ([Attr n] -> Markup n a)

instance (a ~ (), KnownSymbol s) => IsLabel s (MakeNode n a) where
  fromLabel p = N (node (fromString (symbolVal' p)))

instance Bifunctor Markup where
  first f = Markup . hoistF (first f) . unMarkup
  second = fmap

-- | For use with @OverloadedLabels@.
--
-- @ 'attr' #x [a,b,c] = 'node' "x" [a,b,c] @
--
attr :: MakeNode n a -> [Attr n] -> Markup n a
attr (N f) a = f a

runMarkup :: Markup n a -> Markup' n
runMarkup h = runF (unMarkup h) (const Empty) embed

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

{-# INLINE stream #-}
stream :: s -> (s -> Maybe s) -> Markup (s -> n) a -> Markup n a
stream s0 next h =
  case compile h of
    tpl -> tpl `seq` liftF
      (StreamF
       (S s0
        (\s -> case next s of
            Just s' -> Next s' (fmap (\f -> f s) tpl)
            Nothing -> Done (fmap (\f -> f s) tpl))))

enum :: (Ord a, Enum a) => a -> a -> Markup (a -> n) r -> Markup n r
enum start end = stream start
  (\e -> if e <= end
         then Just (succ e)
         else Nothing)

doctype :: Markup n a
doctype = liftF DoctypeF

