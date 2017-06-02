{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
module Text.Html.Nice.Writer where
import           Data.Foldable          (toList)
import           Data.String            (IsString (..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Vector            as V
import           Text.Html.Nice


type Children p = [Markup' p] -> [Markup' p]

data MarkupStep p a = MarkupStep
  { msGlobalId :: {-# UNPACK #-} !Int
  , msChildren :: Children p
  , msResult   :: a
  } deriving (Functor, Foldable, Traversable)

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
compileE :: Markup t a -> (a, FastMarkup t)
compileE m = case runM 0 m of (m', _, a) -> (a, compile_ m')

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

--------------------------------------------------------------------------------
-- Node types

{-# INLINE dynamic #-}
dynamic :: p -> Markup p ()
dynamic = lift . Hole DoEscape

instance a ~ () => IsString (Markup t a) where
  fromString = text . fromString

--------------------------------------------------------------------------------
-- Combinators

{-# INLINE (!) #-}
-- | Add some attributes
(!) :: (Markup t a -> Markup t b) -> [Attr t] -> Markup t a -> Markup t b
(!) f a x = Markup $ \i attrs ->
  case f x of
    Markup m -> m i (a ++ attrs)

-- | Give a node a unique id
--
-- Might be handy to build server-side react-esque systems
note :: (Markup t a -> Markup t b) -> Markup t a -> Markup t Int
note f x = Markup $ \i attrs ->
  case f x of
    Markup m ->
      (m (i + 1) ("id" := T.pack (show i):attrs)) { msResult = i }

infixl 8 !

{-# INLINE stream #-}
stream :: Foldable f
       => Markup (a -> n) r
       -> Markup (f a -> FastMarkup n) ()
stream m = dynamic (\fa -> FStream (ListS (toList fa) (\a -> fmap ($ a) fm)))
  where !fm = compile m

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


