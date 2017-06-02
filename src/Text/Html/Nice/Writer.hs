{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
module Text.Html.Nice.Writer where
import           Data.String          (IsString (..))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           GHC.OverloadedLabels (IsLabel (..))
import           GHC.TypeLits         (KnownSymbol, symbolVal')
import           Text.Html.Nice

newtype Markup p a = Markup ([Attr p] -> ([Markup' p] -> [Markup' p], a))

instance Functor (Markup p) where
  fmap f (Markup m) = Markup (\attr -> fmap f (m attr))

instance Applicative (Markup p) where
  pure a = Markup (\attr -> (id, a))
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

node :: Text -> Markup p a -> Markup p a
node name m =
  Markup $ \attr -> case runM m of
    (cs, a) -> ((:) (Node name (V.fromList attr) cs), a)

lift :: Markup' t -> Markup t ()
lift m' = Markup (\_ -> ((m':), ()))

--------------------------------------------------------------------------------
-- Combinators

-- | Add some attributes
(!) :: (Markup t a -> Markup t a)
    -> [Attr t]
    -> Markup t a
    -> Markup t a
(!) f a x = Markup $ \attrs ->
  case f x of
    Markup m -> m (a ++ attrs)

infixl 8 !

--------------------------------------------------------------------------------
-- Node types

text :: Text -> Markup t ()
text = lift . Text DoEscape . StrictT

dynamic :: p -> Markup p ()
dynamic = lift . Hole DoEscape

-------------------------------------------------------------------------------
-- Elements

instance a ~ () => IsString (Markup t a) where
  fromString = text . fromString
