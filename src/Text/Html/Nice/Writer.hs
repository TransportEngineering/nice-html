{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
module Text.Html.Nice.Writer
  ( -- * The markup monad
    Markup
    -- * Basic node types
  , text
  , lazyText
  , builder
  , string
  , doctype_
    -- ** Variants that don't escape their input
  , textRaw
  , lazyTextRaw
  , builderRaw
  , stringRaw
    -- * Combinators
  , AddAttr
  , (!)
  , dynamic
  , dynamicRaw
  , using
  , sub
  , mapP
    -- ** Streaming
  , stream
    -- ** Noting specific elements
  , Note (..)
  , note
    -- * Compilation
  , compile
  , runMarkup
    -- * Internals
  , makeElement
  , makeVoidElement
  ) where
import           Data.Foldable           (toList)
import           Data.String             (IsString (..))
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Builder  as TLB
import qualified Data.Vector             as V
import           Text.Html.Nice.Internal

type Children p = [Markup' p] -> [Markup' p]

data MarkupStep p a = MarkupStep
  { msGlobalId :: {-# UNPACK #-} !Int
  , msChildren :: Children p
  , msResult   :: a
  } deriving (Functor, Foldable, Traversable)

-- | A Writer-like monad
newtype Markup p a = Markup (Int -> [Attr p] -> MarkupStep p a)

instance Functor (Markup p) where
  fmap f (Markup m) = Markup (\i attr -> fmap f (m i attr))

instance Applicative (Markup p) where
  pure a = Markup (\i _ -> MarkupStep i id a)
  Markup f <*> Markup x = Markup $ \i attr ->
    case f i attr of
      MarkupStep j fnodes f' ->
        case x j [] of
          MarkupStep k xnodes x' ->
            MarkupStep k (fnodes . xnodes) (f' x')

instance Monad (Markup p) where
  Markup mx >>= f = Markup $ \i attr -> case mx i attr of
    MarkupStep j dx a -> case f a of
      Markup f' -> case f' j attr of
        MarkupStep k dy b -> MarkupStep k (dx . dy) b

-- | Compile a 'Markup'. Don't run this multiple times!
compile :: Markup t a -> FastMarkup t
compile m = case runM 0 m of (m', _, _) -> compile_ m'

-- | Compile a 'Markup'. Don't run this multiple times!
--
-- Same as 'compile' but lets you use the result.
runMarkup :: Markup t a -> (a, FastMarkup t)
runMarkup m = case runM 0 m of (m', _, a) -> (a, compile_ m')

--------------------------------------------------------------------------------
-- Internals

runM :: Int -> Markup t a -> (Markup' t, Int, a)
runM i (Markup m) = (List (x []), j, a) where MarkupStep j x a = m i []

{-# INLINE makeElement #-}
makeElement :: Text -> Markup p a -> Markup p a
makeElement name m =
  Markup $ \i attr -> case runM i m of
    (cs, j, a) -> MarkupStep
      { msGlobalId = j
      , msChildren = (:) (Node name (V.fromList attr) cs)
      , msResult = a
      }

{-# INLINE makeVoidElement #-}
makeVoidElement :: Text -> Markup p ()
makeVoidElement name = Markup $ \i attr -> MarkupStep
  { msGlobalId = i
  , msChildren = (:) (VoidNode name (V.fromList attr))
  , msResult = ()
  }

{-# INLINE lift #-}
lift :: Markup' t -> Markup t ()
lift m' = Markup $ \i _ -> MarkupStep
  { msGlobalId = i
  , msChildren = (m':)
  , msResult = ()
  }

doctype_ :: Markup p ()
doctype_ = lift Doctype

--------------------------------------------------------------------------------
-- Node types

{-# INLINE using #-}
using :: ToFastMarkup b => (a -> b) -> Markup (a -> FastMarkup r) ()
using f = dynamic (toFastMarkup . f)

{-# INLINE dynamic #-}
dynamic :: p -> Markup p ()
dynamic = lift . Hole DoEscape

{-# INLINE dynamicRaw #-}
dynamicRaw :: p -> Markup p ()
dynamicRaw = lift . Hole Don'tEscape

-- | Map over the holes in a 'Markup'. This necessarily discards any attributes
-- currently being added that are not static.
mapP :: (a -> b) -> Markup a r -> Markup b r
mapP f (Markup m) = Markup $ \i attr -> case m i (foldr addA [] attr) of
  ms -> ms { msChildren = \cs -> map (fmap f) (msChildren ms []) ++ cs }
  where
    addA ((:=) a b) xs = (a := b) : xs
    addA _          xs = xs

instance a ~ () => IsString (Markup t a) where
  fromString = text . fromString

--------------------------------------------------------------------------------
-- Combinators

type MarkupLike a = a

class AddAttr a t | a -> t where
  addAttr :: a -> Attr t -> a

instance AddAttr (Markup t a -> Markup t b) t where
  addAttr f a x = Markup $ \i attrs ->
    case f x of
      Markup m -> m i (a:attrs)

instance AddAttr (Markup t a) t where
  addAttr f a = Markup $ \i attrs ->
    case f of
      Markup m -> m i (a:attrs)

(!) :: AddAttr a t => MarkupLike a -> Attr t -> MarkupLike a
(!) = addAttr
infixl 8 !

{-# INLINE stream #-}
stream :: Foldable f
       => Markup (a -> n) r
       -> Markup (f a -> FastMarkup n) r
stream m =
  result <$ dynamicRaw (\fa -> FStream (ListS (toList fa) fm))
  where
    (result, !fm) = runMarkup m

-- | Sub-template
sub :: Markup n a -> Markup (FastMarkup n) a
sub m = case runMarkup m of
  (a, fm) -> a <$ lift (Hole Don'tEscape fm)

--------------------------------------------------------------------------------
-- 'Note' system

data Note a = Note
  { noteId :: {-# UNPACK #-} !Int
  , noted  :: FastMarkup a
  } deriving (Eq, Show, Functor)

-- | Give a node a unique id
--
-- Might be handy to build server-side react-esque systems
note :: (Markup t a -> Markup t b) -> Markup t a -> Markup t (Note t, b)
note f x = withNote
  where
    withNote = do
      (i, a) <- markup
      case runM i markup of
        (m', _, _) -> return (Note
          { noteId = i
          , noted = compile_ m'
          }, a)

    markup = Markup $ \i attrs -> case f x of
      Markup m ->
        case m (i + 1) ("id" := niceId i:attrs) of
          ms -> ms { msResult = (i, msResult ms) }

-- | HTML 'id' attribute given to 'note'd elements.
niceId :: Int -> Text
niceId i = T.pack ("nice-" ++ show i)

--------------------------------------------------------------------------------
-- Useful string functions

-- | Insert text and escape it
text :: Text -> Markup t ()
text = lift . Text DoEscape . StrictT

-- | Insert text and escape it
lazyText :: TL.Text -> Markup n ()
lazyText = lift . Text DoEscape . LazyT

-- | Insert text and escape it
builder :: TLB.Builder -> Markup n ()
builder = lift . Text DoEscape . BuilderT

-- | Insert text and escape it
string :: String -> Markup n ()
string = lift . Text DoEscape . BuilderT . TLB.fromString

-- | Insert text and don't escape it
textRaw :: Text -> Markup t ()
textRaw = lift . Text Don'tEscape . StrictT

-- | Insert text and don't escape it
lazyTextRaw :: TL.Text -> Markup n ()
lazyTextRaw = lift . Text Don'tEscape . LazyT

-- | Insert text and don't escape it
builderRaw :: TLB.Builder -> Markup n ()
builderRaw = lift . Text Don'tEscape . BuilderT

-- | Insert text and don't escape it
stringRaw :: String -> Markup n ()
stringRaw = lift . Text Don'tEscape . BuilderT . TLB.fromString

