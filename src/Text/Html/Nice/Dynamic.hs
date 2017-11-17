{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Text.Html.Nice.Dynamic where
import           Control.Lens                      hiding (children)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Writer.Strict
import           Data.Foldable                     (toList)
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as M
import           Data.Sequence                     (Seq)
import qualified Data.Sequence                     as S
import           Data.String                       (IsString (..))
import           Data.Text                         (Text)
import qualified Data.Vector                       as V
import           Text.Html.Nice                    (Attr (..), FastMarkup)
import qualified Text.Html.Nice.Internal           as Nice

--------------------------------------------------------------------------------
-- Core types

data LocalMarkupState a = LocalMarkupState
  { _attrs    :: [Attr a]
  , _children :: Seq (Nice.Markup' a)
  }

data DynamicFastMarkup a where
  DynamicFastMarkup :: (a -> b)
                    -> FastMarkup b
                    -> DynamicFastMarkup a

data MarkupState a = MarkupState
  { _nextId     :: {-# UNPACK #-} !Int
  , _idToMarkup :: Map Int (DynamicFastMarkup a)
  , _localState :: {-# UNPACK #-} !(LocalMarkupState a)
  }

newtype Markup p a = Markup
  { runMarkup :: State (MarkupState p) a }
  deriving (Functor, Applicative, Monad)

makeLenses ''LocalMarkupState
makeLenses ''MarkupState

instance a ~ () => IsString (Markup p a) where
  fromString = text . fromString

--------------------------------------------------------------------------------

startingLocalMarkupState :: LocalMarkupState a
startingLocalMarkupState = LocalMarkupState [] S.empty

startingMarkupState :: MarkupState a
startingMarkupState = MarkupState
  { _nextId = 1
  , _idToMarkup = mempty
  , _localState = startingLocalMarkupState
  }

compile :: Markup p a -> FastMarkup p
compile (Markup s) = Nice.compile_ . Nice.List . toList $
  execState s startingMarkupState^.localState.children

--------------------------------------------------------------------------------
-- Internal combinators

data Id = Id !Text {-# UNPACK #-} !Int

getNextId :: Markup p Id
getNextId =
  (\i -> Id (fromString (show i)) i) <$> Markup (nextId <<+= 1)

isolate :: Markup p a -> Markup p (LocalMarkupState p, a)
isolate f = Markup $ do
  z <- localState <<.= startingLocalMarkupState
  r <- runMarkup f
  s <- use localState
  localState .= z
  return (s, r)

liftM' :: Nice.Markup' p -> Markup p ()
liftM' m = Markup (localState.children %= (<|) m)

makeElement :: Text -> Markup p a -> Markup p a
makeElement name children' = Markup $ do
  attrsNow <- localState.attrs <<.= []
  (s, r) <- runMarkup (isolate children')
  localState.children %= \cs -> cs |>
    Nice.Node name (V.fromList attrsNow) (Nice.List (toList (_children s)))
  return r

--------------------------------------------------------------------------------

(!) :: (Markup p a -> Markup p a) -> Attr p -> Markup p a -> Markup p a
(!) f attr x = Markup $ do
  z <- localState.attrs <<%= (attr:)
  r <- runMarkup (f x)
  localState.attrs .= z
  return r

infixl 8 !

--------------------------------------------------------------------------------
-- Inputs

newtype Dyn a = Dyn a

{-
dyn :: (Markup a r -> Markup a r)
    -> (a -> b)
    -> _
dyn wrapper sel = case compile (hole sel) of
  c -> c `seq` do
    Id idText idInt <- getNextId
    wrapper ! "id" := idText $ do
      Markup $ idToMarkup.at idInt ?= DynamicFastMarkup sel c
      liftM' $ Nice.Precompiled c
-}

hole :: a -> Markup a ()
hole = liftM' . Nice.Hole Nice.DoEscape

holeRaw :: a -> Markup a ()
holeRaw = liftM' . Nice.Hole Nice.Don'tEscape

--------------------------------------------------------------------------------
-- String functions

text :: Text -> Markup p ()
text = liftM' . Nice.Text Nice.DoEscape . Nice.StrictT

