{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Text.Html.Nice.Internal where
import           Control.DeepSeq                  (NFData (..))
import           Control.Monad
import           Control.Monad.Trans.Reader       (ReaderT (..))
import           Data.Bifunctor.TH
import           Data.Functor.Foldable.TH
import           Data.Functor.Identity
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Builder           as TLB
import qualified Data.Text.Lazy.Builder.Int       as TLB
import qualified Data.Text.Lazy.Builder.RealFloat as TLB
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as V
import           Data.Void
import           GHC.Generics                     (Generic)
import qualified Text.Blaze                       as Blaze
import qualified Text.Blaze.Internal              as Blaze (textBuilder)
import qualified Text.Blaze.Renderer.Text         as Blaze

type AttrName = Text

data Attr a = (:=)
  { attrKey :: !AttrName
  , attrVal :: !Text
  } | (:-)
  { attrKey     :: !AttrName
  , attrValHole :: a
  } deriving (Show, Eq, Functor, Foldable, Traversable)

data IsEscaped = DoEscape | Don'tEscape
  deriving (Show, Eq)

data SomeText
  = LazyT TL.Text
  | BuilderT TLB.Builder
  | StrictT !T.Text
  deriving (Show, Eq)

-- | A very simple HTML DSL
data Markup' a
  = Doctype
  | Node !Text !(Vector (Attr a)) (Markup' a)
  | VoidNode !Text !(Vector (Attr a))
  | List [Markup' a]
  | Stream (Stream a)
  | Text !IsEscaped !SomeText
  | Hole !IsEscaped a
  | Precompiled !(FastMarkup a)
  | Empty
  deriving (Show, Eq, Functor, Foldable) {- , Traversable) -}

data Stream a = forall s. ListS [s] !(FastMarkup (s -> a))

instance Show a => Show (Stream a) where
  show (ListS s f) = "(Stream (" ++ show (map (\a -> fmap ($ a) f) s) ++ "))"

-- | Don't use this! It's a lie!
instance Eq (Stream a) where
  _ == _ = True

instance Functor Stream where
  fmap f (ListS l g) = ListS l (fmap (fmap f) g)

instance Foldable Stream where
  foldMap f s = unstream (foldMap f) s mappend mempty

instance NFData a => NFData (Stream a) where
  rnf (ListS !_ !s) = rnf s

{-# INLINE unstream #-}
unstream :: (FastMarkup a -> b) -> Stream a -> (b -> c -> c) -> c -> c
unstream f (ListS l fm) cons nil =
  foldr (\x -> cons (f (fmap ($ x) fm))) nil l

--------------------------------------------------------------------------------
-- Compiling

data a :$ b = (:$) (FastMarkup (a -> b)) a
  deriving (Functor)

infixl 0 :$

instance Show a => Show (a :$ b) where
  show (a :$ b) = '(':showsPrec 11 b (' ':':':'$':' ':showsPrec 11 (b <$ a) ")")

data FastMarkup a
  = Bunch {-# UNPACK #-} !(Vector (FastMarkup a))
    -- could unpack this manually but would then have to also write
    -- Show/Eq/Functor/Foldable manually
  | FStream !(Stream a)
  | FHole !IsEscaped !a
  | FLText TL.Text
  | FSText {-# UNPACK #-} !Text
  | FBuilder !TLB.Builder
  | FEmpty
  | FDeep (FastMarkup (FastMarkup a))
  deriving (Show, Eq, Functor, Foldable, Generic)

instance Monoid (FastMarkup a) where
  mempty = FBuilder mempty
  mappend a b = Bunch [a, b]

instance NFData a => NFData (FastMarkup a) where
  rnf f = case f of
    Bunch v    -> rnf v
    FStream s  -> rnf s
    FLText t   -> rnf t
    FSText t   -> rnf t
    FHole !_ a -> rnf a
    _          -> ()

makeBaseFunctor ''Markup'
deriveBifunctor ''Markup'F

{-# INLINE plateFM #-}
-- | Unlike 'plate', this uses 'Monad'. That's because 'traverse' over 'Vector'
-- is really quite slow.
plateFM :: Monad m
        => (FastMarkup a -> m (FastMarkup a))
        -> FastMarkup a
        -> m (FastMarkup a)
plateFM f x = case x of
  Bunch v -> Bunch <$> V.mapM f v
  _       -> pure x

compileAttrs :: forall a. Vector (Attr a) -> (TLB.Builder, Vector (Attr a))
compileAttrs v = (static, dynAttrs)
  where
    isHoly :: Foldable f => f a -> Bool
    isHoly = foldr (\_ _ -> True) False

    staticAttrs :: Vector (Attr a)
    dynAttrs :: Vector (Attr a)
    (dynAttrs, staticAttrs) = case V.unstablePartition isHoly v of
      (dyn, stat) -> (dyn, stat)

    static :: TLB.Builder
    static =
      V.foldr
      (\((:=) key val) xs ->
         " " <> TLB.fromText key <> "=\"" <> escapeText val <> "\"" <> xs)
      mempty
      staticAttrs

escapeText :: Text -> TLB.Builder
escapeText = Blaze.renderMarkupBuilder . Blaze.text

{-# INLINE escape #-}
escape :: SomeText -> TLB.Builder
escape st = case st of
  StrictT t  -> Blaze.renderMarkupBuilder (Blaze.text t)
  LazyT t    -> Blaze.renderMarkupBuilder (Blaze.lazyText t)
  BuilderT t -> Blaze.renderMarkupBuilder (Blaze.textBuilder t)

toText :: TLB.Builder -> Text
toText = TL.toStrict . TLB.toLazyText

fastAttr :: Attr a -> FastMarkup a
fastAttr ((:-) k v) =
  Bunch [FSText (" " <> k <> "=\""), FHole DoEscape v, FSText "\""]
fastAttr _ =
  error "very bad"

fast :: Markup' a -> FastMarkup a
fast m = case m of
  Doctype -> FSText "<!DOCTYPE html>\n"
  Node t attrs m' -> case compileAttrs attrs of
    (staticAttrs, dynAttrs) -> case V.length dynAttrs of
      0 -> Bunch
        [ FSText (T.concat ["<", t, toText staticAttrs, ">"])
        , fast m'
        , FSText (T.concat ["</", t, ">"])
        ]
      _ -> Bunch
        [ FBuilder ("<" <> TLB.fromText t <> staticAttrs)
        , Bunch (V.map fastAttr dynAttrs)
        , FSText ">"
        , fast m'
        , FSText ("</" <>  t <> ">")
        ]
  VoidNode t attrs -> case compileAttrs attrs of
    (staticAttrs, dynAttrs) -> case V.length dynAttrs of
      0 -> FSText (T.concat ["<", t, toText staticAttrs, " />"])
      _ -> Bunch
           [ FBuilder ("<" <> TLB.fromText t <> staticAttrs)
           , Bunch (V.map fastAttr dynAttrs)
           , FSText " />"
           ]
  Text DoEscape t -> FBuilder (escape t)
  Text Don'tEscape t -> case t of
    StrictT a  -> FSText a
    LazyT a    -> FLText a
    BuilderT a -> FBuilder a
  List v -> Bunch (V.map fast (V.fromList v))
  Hole e v -> FHole e v
  Stream a -> FStream a
  Empty -> FEmpty
  Precompiled fm -> fm

-- | Look for an immediate string-like term and render that
immediateRender :: FastMarkup a -> Maybe TLB.Builder
immediateRender fm = case fm of
  FBuilder t -> Just t
  FSText t   -> Just (TLB.fromText t)
  FLText t   -> Just (TLB.fromLazyText t)
  FEmpty     -> Just mempty
  _          -> Nothing

-- | Flatten a vector of 'FastMarkup. String-like terms that are next to
-- eachother should be combined
munch :: Vector (FastMarkup a) -> Vector (FastMarkup a)
munch v = V.fromList (go mempty 0)
  where
    len = V.length v
    go acc i
      | i < len =
        let e = V.unsafeIndex v i
        in case immediateRender e of
          Just b  -> go (acc <> b) (i + 1)
          Nothing -> FBuilder acc : e : go mempty (i + 1)
      | otherwise = [FBuilder acc]

-- | Recursively flatten 'FastMarkup' until doing so does nothing
flatten :: FastMarkup a -> FastMarkup a
flatten fm = case fm of
  FStream t -> FStream t -- ignore streams
  _         -> go

  where
    go = again $ case fm of
      Bunch v -> case V.length v of
        0 -> FEmpty
        1 -> V.head v
        _ -> Bunch
         (munch
          (V.concatMap
           (\x -> case x of
               Bunch v' -> v'
               FEmpty   -> V.empty
               _        -> V.singleton (flatten x))
           v))
      _ -> runIdentity (plateFM (Identity . flatten) fm)

    again a
      -- Eq but ignore holes
      | (() <$ a) == (() <$ fm) = fm
      | otherwise = flatten a

-- | Run all Text builders
strictify :: FastMarkup a -> FastMarkup a
strictify fm = case fm of
  FBuilder t -> FLText (TLB.toLazyText t)
  FLText   t -> FLText t
  _          -> runIdentity (plateFM (Identity . strictify) fm)

-- | Compile 'Markup'''
compile_ :: Markup' a -> FastMarkup a
compile_ = strictify . flatten . fast

recompile :: FastMarkup a -> FastMarkup a
recompile = strictify . flatten

unlayer :: FastMarkup (FastMarkup a) -> FastMarkup a
unlayer = FDeep

--------------------------------------------------------------------------------
-- Rendering

{-#
  SPECIALISE renderM :: (a -> Identity TLB.Builder)
                     -> FastMarkup a
                     -> Identity TLB.Builder
  #-}

{-# INLINE renderM #-}
-- | Render 'FastMarkup'
renderM :: Monad m => (a -> m TLB.Builder) -> FastMarkup a -> m TLB.Builder
renderM f = go
  where
    go fm = case fm of
      Bunch v     -> V.foldM (\acc x -> mappend acc <$> go x) mempty v
      FBuilder t  -> return t
      FSText t    -> return (TLB.fromText t)
      FLText t    -> return (TLB.fromLazyText t)
      FHole esc a -> case esc of
        DoEscape    -> Blaze.renderMarkupBuilder . Blaze.textBuilder <$> f a
        Don'tEscape -> f a
      FStream str -> unstream go str (liftM2 mappend) (return mempty)
      FDeep   a   -> renderM go a
      _           -> return mempty

{-# INLINE renderMs #-}
-- | Render 'FastMarkup' by recursively rendering any sub-markup.
renderMs :: Monad m => (a -> m (FastMarkup Void)) -> FastMarkup a -> m TLB.Builder
renderMs f = renderM (f >=> renderMs (f . absurd))

{-# INLINE render #-}
-- | Render 'FastMarkup' that has no holes.
render :: FastMarkup Void -> TLB.Builder
render = runIdentity . renderM absurd

{-# INLINE r_ #-}
r_ :: Render a Identity => a -> TLB.Builder
r_ = runIdentity . r

class Render a m where
  r :: a -> m TLB.Builder

-- needs undecidableinstances ...
instance (Render b m, m' ~ ReaderT a m) => Render (a -> b) m' where
  {-# INLINE r #-}
  r f = ReaderT (r . f)

-- | Defer application of an argument to rendering
instance (Monad m, Render b m) => Render (a :$ b) m where
  {-# INLINE r #-}
  r (b :$ a) = renderM (\f -> r (f a)) b

instance Monad m => Render Void m where
  {-# INLINE r #-}
  r = return . absurd

instance Monad m => Render TLB.Builder m where
  {-# INLINE r #-}
  r = return

instance Monad m => Render T.Text m where
  {-# INLINE r #-}
  r = return . TLB.fromText

instance Monad m => Render TL.Text m where
  {-# INLINE r #-}
  r = return . TLB.fromLazyText

instance {-# OVERLAPPABLE #-} (Render a m, Monad m) => Render (FastMarkup a) m where
  {-# INLINE r #-}
  r = renderM r

newtype RenderToFastMarkup a = RenderToFastMarkup { unToFastMarkup :: a }

instance (ToFastMarkup a, Monad m) => Render (RenderToFastMarkup a) m where
  {-# INLINE r #-}
  r = r . (toFastMarkup :: a -> FastMarkup Void) . unToFastMarkup

--------------------------------------------------------------------------------

class ToFastMarkup a where
  toFastMarkup :: a -> FastMarkup b

instance ToFastMarkup (FastMarkup Void) where
  toFastMarkup = vacuous

instance ToFastMarkup Text where
  {-# INLINE toFastMarkup #-}
  toFastMarkup = FSText

instance ToFastMarkup TL.Text where
  {-# INLINE toFastMarkup #-}
  toFastMarkup = FLText

instance ToFastMarkup TLB.Builder where
  {-# INLINE toFastMarkup #-}
  toFastMarkup = FBuilder

newtype AsDecimal a = AsDecimal { asDecimal :: a }
instance Integral a => ToFastMarkup (AsDecimal a) where
  {-# INLINE toFastMarkup #-}
  toFastMarkup = toFastMarkup . TLB.decimal . asDecimal

newtype AsHex a = AsHex { asHex :: a }
instance Integral a => ToFastMarkup (AsHex a) where
  {-# INLINE toFastMarkup #-}
  toFastMarkup = toFastMarkup . TLB.hexadecimal . asHex

newtype AsRealFloat a = AsRealFloat { asRealFloat :: a }
instance RealFloat a => ToFastMarkup (AsRealFloat a) where
  {-# INLINE toFastMarkup #-}
  toFastMarkup = toFastMarkup . TLB.realFloat . asRealFloat
