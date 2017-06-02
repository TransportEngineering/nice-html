{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DataKinds              #-}
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
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Vector            as V
import           Text.Html.Nice

newtype Markup p a = Markup ([Attr p] -> ([Markup' p] -> [Markup' p], a))

instance Functor (Markup p) where
  fmap f (Markup m) = Markup (\attr -> fmap f (m attr))

instance Applicative (Markup p) where
  pure a = Markup (\_ -> (id, a))
  Markup f <*> Markup x = Markup $ \attr ->
    case f attr of
      (fnodes, f') -> case x [] of
        (xnodes, x') -> (fnodes . xnodes, f' x')

instance Monad (Markup p) where
  Markup mx >>= f = Markup $ \attr -> case mx attr of
    (dx, a) -> case f a of
      Markup a' -> case a' [] of
        (dy, b) -> (dx . dy, b)

-- | Compile a 'Markup'. Don't run this multiple times!
compile :: Markup t a -> FastMarkup t
compile m = case runM m of (m', _) -> compile_ m'

--------------------------------------------------------------------------------
-- Internals

runM :: Markup t a -> (Markup' t, a)
runM (Markup m) = (List (x []), a) where (x, a) = m []

{-# INLINE makeElement #-}
makeElement :: Text -> Markup p a -> Markup p a
makeElement name m =
  Markup $ \attr -> case runM m of
    (cs, a) -> ((:) (Node name (V.fromList attr) cs), a)

{-# INLINE makeVoidElement #-}
makeVoidElement :: Text -> Markup p ()
makeVoidElement name =
  Markup $ \attr -> ((:) (VoidNode name (V.fromList attr)), ())

{-# INLINE lift #-}
lift :: Markup' t -> Markup t ()
lift m' = Markup (\_ -> ((m':), ()))

--------------------------------------------------------------------------------
-- Node types

{-# INLINE dynamic #-}
dynamic :: p -> Markup p ()
dynamic = lift . Hole DoEscape

instance a ~ () => IsString (Markup t a) where
  fromString = text . fromString

--------------------------------------------------------------------------------
-- Combinators

type Node t a = Markup t a -> Markup t a

{-# INLINE (!) #-}
-- | Add some attributes
(!) :: Node t a -> [Attr t] -> Markup t a -> Markup t a
(!) f a x = Markup $ \attrs ->
  case f x of
    Markup m -> m (a ++ attrs)

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


