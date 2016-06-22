{-|
Module      : $Header$
Description : Types and conversions
Copyright   : (c) Justus Adam, 2015
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE TemplateHaskell       #-}
module Text.Mustache.Types
  (
  -- * Types for the Parser / Template
    STree
  , Template(..)
  , Node(..)
  , DataIdentifier(..)
  -- * Types for the Substitution / Data
  , Value(..)
  , Key
  -- ** Converting
  , object
  , (~>), (↝), (~=), (⥱)
  , ToMustache, toMustache, mFromJSON
  -- ** Representation
  , Array, Object, Pair
  , Context(..)
  , TemplateCache
  ) where


import qualified Data.Aeson               as Aeson
import           Data.HashMap.Strict      as HM
import qualified Data.HashSet             as HS
import qualified Data.Map                 as Map
import           Data.Scientific
import           Data.Text
import qualified Data.Text.Lazy           as LT
import qualified Data.Vector              as V
import           Language.Haskell.TH.Lift (Lift(lift), deriveLift)
import           Prelude.Unicode

-- | Syntax tree for a mustache template
type STree = ASTree Text


type ASTree α = [Node α]


-- | Basic values composing the STree
data Node α
  = TextBlock α
  | Section DataIdentifier (ASTree α)
  | InvertedSection DataIdentifier (ASTree α)
  | Variable Bool DataIdentifier
  | Partial (Maybe α) FilePath
  deriving (Show, Eq)


-- | Kinds of identifiers for Variables and sections
data DataIdentifier
  = NamedData [Key]
  | Implicit
  deriving (Show, Eq)


-- | A list-like structure used in 'Value'
type Array  = V.Vector Value
-- | A map-like structure used in 'Value'
type Object = HM.HashMap Text Value
-- | Source type for constructing 'Object's
type Pair   = (Text, Value)


-- | Representation of stateful context for the substitution process
data Context α = Context [α] α
  deriving (Eq, Show, Ord)

-- | Internal value representation
data Value
  = Object !Object
  | Array  !Array
  | Number !Scientific
  | String !Text
  | Lambda (Context Value → STree → STree)
  | Bool   !Bool
  | Null


instance Show Value where
  show (Lambda _) = "Lambda function"
  show (Object o) = show o
  show (Array  a) = show a
  show (String s) = show s
  show (Number n) = show n
  show (Bool   b) = show b
  show Null       = "null"

-- | Conversion class
class ToMustache ω where
  toMustache ∷ ω → Value
  listToMustache ∷ [ω] → Value
  listToMustache = Array ∘ V.fromList ∘ fmap toMustache

instance ToMustache Float where
  toMustache = Number ∘ fromFloatDigits

instance ToMustache Double where
  toMustache = Number ∘ fromFloatDigits

instance ToMustache Integer where
  toMustache = Number ∘ fromInteger

instance ToMustache Int where
  toMustache = toMustache ∘ toInteger

instance ToMustache Char where
  toMustache = toMustache ∘ (:[])
  listToMustache = String ∘ pack

instance ToMustache Value where
  toMustache = id

instance ToMustache Bool where
  toMustache = Bool

instance ToMustache () where
  toMustache = const Null

instance ToMustache Text where
  toMustache = String

instance ToMustache LT.Text where
  toMustache = String ∘ LT.toStrict

instance ToMustache Scientific where
  toMustache = Number

instance ToMustache α ⇒ ToMustache [α] where
  toMustache = listToMustache

instance ToMustache ω ⇒ ToMustache (V.Vector ω) where
  toMustache = toMustache ∘ fmap toMustache

instance (ToMustache ω) ⇒ ToMustache (Map.Map Text ω) where
  toMustache = mapInstanceHelper id

instance (ToMustache ω) ⇒ ToMustache (Map.Map LT.Text ω) where
  toMustache = mapInstanceHelper LT.toStrict

instance (ToMustache ω) ⇒ ToMustache (Map.Map String ω) where
  toMustache = mapInstanceHelper pack

mapInstanceHelper :: ToMustache v => (a -> Text) -> Map.Map a v -> Value
mapInstanceHelper conv =
  toMustache
  ∘ Map.foldrWithKey
    (\k → HM.insert (conv k) ∘ toMustache)
    HM.empty

instance ToMustache ω ⇒ ToMustache (HM.HashMap Text ω) where
  toMustache = toMustache ∘ fmap toMustache

instance ToMustache ω ⇒ ToMustache (HM.HashMap LT.Text ω) where
  toMustache = hashMapInstanceHelper LT.toStrict

instance ToMustache ω ⇒ ToMustache (HM.HashMap String ω) where
  toMustache = hashMapInstanceHelper pack

hashMapInstanceHelper :: ToMustache v => (a -> Text) -> HM.HashMap a v -> Value
hashMapInstanceHelper conv =
  toMustache
  ∘ HM.foldrWithKey
    (\k → HM.insert (conv k) ∘ toMustache)
    HM.empty

instance ToMustache (Context Value → STree → STree) where
  toMustache = Lambda

instance ToMustache (Context Value → STree → Text) where
  toMustache = lambdaInstanceHelper id

instance ToMustache (Context Value → STree → LT.Text) where
  toMustache = lambdaInstanceHelper LT.toStrict

instance ToMustache (Context Value → STree → String) where
  toMustache = lambdaInstanceHelper pack

lambdaInstanceHelper :: (a -> Text) -> (Context Value -> STree -> a) -> Value
lambdaInstanceHelper conv f = Lambda wrapper
  where
    wrapper ∷ Context Value → STree → STree
    wrapper c lSTree = return ∘ TextBlock $ conv $ f c lSTree

instance ToMustache (STree → STree) where
  toMustache f = toMustache (const f ∷ Context Value → STree → STree)

instance ToMustache (STree → Text) where
  toMustache f = toMustache wrapper
    where
      wrapper ∷ Context Value → STree → STree
      wrapper _ = (return ∘ TextBlock) ∘ f

instance ToMustache Aeson.Value where
  toMustache (Aeson.Object o) = Object $ fmap toMustache o
  toMustache (Aeson.Array  a) = Array $ fmap toMustache a
  toMustache (Aeson.Number n) = Number n
  toMustache (Aeson.String s) = String s
  toMustache (Aeson.Bool   b) = Bool b
  toMustache Aeson.Null       = Null

instance ToMustache ω ⇒ ToMustache (HS.HashSet ω) where
  toMustache = toMustache ∘ HS.toList

instance (ToMustache α, ToMustache β) ⇒ ToMustache (α, β) where
  toMustache (a, b) = toMustache [toMustache a, toMustache b]

instance (ToMustache α, ToMustache β, ToMustache γ)
         ⇒ ToMustache (α, β, γ) where
  toMustache (a, b, c) = toMustache [toMustache a, toMustache b, toMustache c]

instance (ToMustache α, ToMustache β, ToMustache γ, ToMustache δ)
         ⇒ ToMustache (α, β, γ, δ) where
  toMustache (a, b, c, d) = toMustache
    [ toMustache a
    , toMustache b
    , toMustache c
    , toMustache d
    ]

instance ( ToMustache α
         , ToMustache β
         , ToMustache γ
         , ToMustache δ
         , ToMustache ε
         ) ⇒ ToMustache (α, β, γ, δ, ε) where
  toMustache (a, b, c, d, e) = toMustache
    [ toMustache a
    , toMustache b
    , toMustache c
    , toMustache d
    , toMustache e
    ]

instance ( ToMustache α
         , ToMustache β
         , ToMustache γ
         , ToMustache δ
         , ToMustache ε
         , ToMustache ζ
         ) ⇒ ToMustache (α, β, γ, δ, ε, ζ) where
  toMustache (a, b, c, d, e, f) = toMustache
    [ toMustache a
    , toMustache b
    , toMustache c
    , toMustache d
    , toMustache e
    , toMustache f
    ]

instance ( ToMustache α
         , ToMustache β
         , ToMustache γ
         , ToMustache δ
         , ToMustache ε
         , ToMustache ζ
         , ToMustache η
         ) ⇒ ToMustache (α, β, γ, δ, ε, ζ, η) where
  toMustache (a, b, c, d, e, f, g) = toMustache
    [ toMustache a
    , toMustache b
    , toMustache c
    , toMustache d
    , toMustache e
    , toMustache f
    , toMustache g
    ]

instance ( ToMustache α
         , ToMustache β
         , ToMustache γ
         , ToMustache δ
         , ToMustache ε
         , ToMustache ζ
         , ToMustache η
         , ToMustache θ
         ) ⇒ ToMustache (α, β, γ, δ, ε, ζ, η, θ) where
  toMustache (a, b, c, d, e, f, g, h) = toMustache
    [ toMustache a
    , toMustache b
    , toMustache c
    , toMustache d
    , toMustache e
    , toMustache f
    , toMustache g
    , toMustache h
    ]

-- | Convenience function for creating Object values.
--
-- This function is supposed to be used in conjuction with the '~>' and '~=' operators.
--
-- ==== __Examples__
--
-- @
--   data Address = Address { ... }
--
--   instance Address ToJSON where
--     ...
--
--   data Person = Person { name :: String, address :: Address }
--
--   instance ToMustache Person where
--     toMustache (Person { name, address }) = object
--       [ "name" ~> name
--       , "address" ~= address
--       ]
-- @
--
-- Here we can see that we can use the '~>' operator for values that have
-- themselves a 'ToMustache' instance, or alternatively if they lack such an
-- instance but provide an instance for the 'ToJSON' typeclass we can use the
-- '~=' operator.
object ∷ [Pair] → Value
object = Object ∘ HM.fromList


-- | Map keys to values that provide a 'ToMustache' instance
--
-- Recommended in conjunction with the `OverloadedStrings` extension.
(~>) ∷ ToMustache ω ⇒ Text → ω → Pair
(~>) t = (t, ) ∘ toMustache
{-# INLINEABLE (~>) #-}
infixr 8 ~>

-- | Unicode version of '~>'
(↝) ∷ ToMustache ω ⇒ Text → ω → Pair
(↝) = (~>)
{-# INLINEABLE (↝) #-}
infixr 8 ↝


-- | Map keys to values that provide a 'ToJSON' instance
--
-- Recommended in conjunction with the `OverloadedStrings` extension.
(~=) ∷ Aeson.ToJSON ι ⇒ Text → ι → Pair
(~=) t = (t ~>) ∘ Aeson.toJSON
{-# INLINEABLE (~=) #-}
infixr 8 ~=


-- | Unicode version of '~='
(⥱) ∷ Aeson.ToJSON ι ⇒ Text → ι → Pair
(⥱) = (~=)
{-# INLINEABLE (⥱) #-}
infixr 8 ⥱


-- | Converts a value that can be represented as JSON to a Value.
mFromJSON ∷ Aeson.ToJSON ι ⇒ ι → Value
mFromJSON = toMustache ∘ Aeson.toJSON


-- | A collection of templates with quick access via their hashed names
type TemplateCache = HM.HashMap String Template

-- | Type of key used for retrieving data from 'Value's
type Key = Text

{-|
  A compiled Template with metadata.
-}
data Template = Template
  { name     ∷ String
  , ast      ∷ STree
  , partials ∷ TemplateCache
  } deriving (Show)

instance Lift TemplateCache where
  lift = const [|HM.empty|]

instance Lift Text where
  lift = lift ∘ unpack

deriveLift ''DataIdentifier
deriveLift ''Node
deriveLift ''Template
