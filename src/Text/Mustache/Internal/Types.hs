{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
module Text.Mustache.Internal.Types where


import           Control.Arrow
import           Control.Monad.RWS        hiding (lift)
import qualified Data.Aeson               as Aeson
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap        as KM
#endif
import           Data.Int                 (Int8, Int16, Int32, Int64)
import           Data.Foldable            (toList)
import qualified Data.HashMap.Strict      as HM
import qualified Data.HashSet             as HS
import qualified Data.Map                 as Map
import           Data.Scientific
import qualified Data.Sequence            as Seq
import qualified Data.Set                 as Set
import           Data.Text
import qualified Data.Text.Lazy           as LT
import qualified Data.Vector              as V
import           Data.Word                (Word8, Word16, Word32, Word64)
import           Language.Haskell.TH.Lift (deriveLift)
import           Numeric.Natural          (Natural)


-- | Type of errors we may encounter during substitution.
data SubstitutionError
  = VariableNotFound [Key] -- ^ The template contained a variable for which there was no data counterpart in the current context
  | InvalidImplicitSectionContextType String -- ^ When substituting an implicit section the current context had an unsubstitutable type
  | InvertedImplicitSection -- ^ Inverted implicit sections should never occur
  | SectionTargetNotFound [Key] -- ^ The template contained a section for which there was no data counterpart in the current context
  | PartialNotFound FilePath -- ^ The template contained a partial for which there was no data counterpart in the current context
  | DirectlyRenderedValue Value -- ^ A complex value such as an Object or Array was directly rendered into the template (warning)
  deriving (Show)


tellError :: SubstitutionError -> SubM ()
tellError e = SubM $ tell ([e], [])


tellSuccess :: Text -> SubM ()
tellSuccess s = SubM $ tell ([], [s])


newtype SubM a = SubM { runSubM' :: RWS (Context Value, TemplateCache) ([SubstitutionError], [Text]) ()  a } deriving (Monad, Functor, Applicative, MonadReader (Context Value, TemplateCache))

runSubM :: SubM a -> Context Value -> TemplateCache -> ([SubstitutionError], [Text])
runSubM comp ctx cache = snd $ evalRWS (runSubM' comp) (ctx, cache) ()

shiftContext :: Context Value -> SubM a -> SubM a
shiftContext = local . first . const

-- | Search for a key in the current context.
--
-- The search is conducted inside out mening the current focus
-- is searched first. If the key is not found the outer scopes are recursively
-- searched until the key is found, then 'innerSearch' is called on the result.
search :: [Key] -> SubM (Maybe Value)
search [] = return Nothing
search (key:nextKeys) = (>>= innerSearch nextKeys) <$> go
  where
    go = asks fst >>= \case
      Context parents focus -> do
        let searchParents = case parents of
                  (newFocus: newParents) -> shiftContext (Context newParents newFocus) $ go
                  _ -> return Nothing
        case focus of
          Object o ->
            case HM.lookup key o of
              Just res -> return $ Just res
              _ -> searchParents
          _ -> searchParents


-- | Searches nested scopes navigating inward. Fails if it encunters something
-- other than an object before the key is expended.
innerSearch :: [Key] -> Value -> Maybe Value
innerSearch []     v          = Just v
innerSearch (y:ys) (Object o) = HM.lookup y o >>= innerSearch ys
innerSearch _      _          = Nothing



-- | Syntax tree for a mustache template
type STree = ASTree Text


type ASTree α = [Node α]


-- | Basic values composing the STree
data Node α
  = TextBlock α
  | Section DataIdentifier (ASTree α)
  | ExistingSection DataIdentifier (ASTree α)
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
data Context α = Context { ctxtParents :: [α], ctxtFocus :: α }
  deriving (Eq, Show, Ord)

-- | Internal value representation
data Value
  = Object !Object
  | Array  !Array
  | Number !Scientific
  | String !Text
  | Lambda (STree -> SubM STree)
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


listToMustache' :: ToMustache ω => [ω] -> Value
listToMustache' = Array . V.fromList . fmap toMustache

integralToMustache :: Integral ω => ω -> Value
integralToMustache = toMustache . toInteger

-- | Conversion class
class ToMustache ω where
  toMustache :: ω -> Value
  listToMustache :: [ω] -> Value
  listToMustache = listToMustache'

instance ToMustache Float where
  toMustache = Number . fromFloatDigits

instance ToMustache Double where
  toMustache = Number . fromFloatDigits

instance ToMustache Integer where
  toMustache = Number . fromInteger

instance ToMustache Natural where
  toMustache = integralToMustache

instance ToMustache Int where
  toMustache = integralToMustache

instance ToMustache Word where
  toMustache = integralToMustache

instance ToMustache Int8 where
  toMustache = integralToMustache

instance ToMustache Int16 where
  toMustache = integralToMustache

instance ToMustache Int32 where
  toMustache = integralToMustache

instance ToMustache Int64 where
  toMustache = integralToMustache

instance ToMustache Word8 where
  toMustache = integralToMustache

instance ToMustache Word16 where
  toMustache = integralToMustache

instance ToMustache Word32 where
  toMustache = integralToMustache

instance ToMustache Word64 where
  toMustache = integralToMustache

instance ToMustache Char where
  toMustache = toMustache . (:[])
  listToMustache = String . pack

instance ToMustache Value where
  toMustache = id

instance ToMustache Bool where
  toMustache = Bool

instance ToMustache () where
  toMustache = const Null

instance ToMustache ω => ToMustache (Maybe ω) where
  toMustache (Just w) = toMustache w
  toMustache Nothing  = Null

instance ToMustache Text where
  toMustache = String

instance ToMustache LT.Text where
  toMustache = String . LT.toStrict

instance ToMustache Scientific where
  toMustache = Number

instance ToMustache α => ToMustache [α] where
  toMustache = listToMustache

instance ToMustache ω => ToMustache (Seq.Seq ω) where
  toMustache = listToMustache' . toList

instance ToMustache ω => ToMustache (V.Vector ω) where
  toMustache = Array . fmap toMustache

instance (ToMustache ω) => ToMustache (Map.Map Text ω) where
  toMustache = mapInstanceHelper id

instance (ToMustache ω) => ToMustache (Map.Map LT.Text ω) where
  toMustache = mapInstanceHelper LT.toStrict

instance (ToMustache ω) => ToMustache (Map.Map String ω) where
  toMustache = mapInstanceHelper pack

mapInstanceHelper :: ToMustache v => (a -> Text) -> Map.Map a v -> Value
mapInstanceHelper conv =
  toMustache
  . Map.foldrWithKey
    (\k -> HM.insert (conv k) . toMustache)
    HM.empty

instance ToMustache ω => ToMustache (HM.HashMap Text ω) where
  toMustache = Object . fmap toMustache

instance ToMustache ω => ToMustache (HM.HashMap LT.Text ω) where
  toMustache = hashMapInstanceHelper LT.toStrict

instance ToMustache ω => ToMustache (HM.HashMap String ω) where
  toMustache = hashMapInstanceHelper pack

hashMapInstanceHelper :: ToMustache v => (a -> Text) -> HM.HashMap a v -> Value
hashMapInstanceHelper conv =
  toMustache
  . HM.foldrWithKey
    (\k -> HM.insert (conv k) . toMustache)
    HM.empty

instance ToMustache (STree -> SubM STree) where
    toMustache = Lambda

instance ToMustache Aeson.Value where
  toMustache (Aeson.Object o) = Object $ fmap toMustache
#if MIN_VERSION_aeson(2,0,0)
    (KM.toHashMapText o)
#else
    o
#endif
  toMustache (Aeson.Array  a) = Array $ fmap toMustache a
  toMustache (Aeson.Number n) = Number n
  toMustache (Aeson.String s) = String s
  toMustache (Aeson.Bool   b) = Bool b
  toMustache Aeson.Null       = Null

instance ToMustache ω => ToMustache (HS.HashSet ω) where
  toMustache = listToMustache' . HS.toList

instance ToMustache ω => ToMustache (Set.Set ω) where
  toMustache = listToMustache' . Set.toList

instance (ToMustache α, ToMustache β) => ToMustache (α, β) where
  toMustache (a, b) = toMustache [toMustache a, toMustache b]

instance (ToMustache α, ToMustache β, ToMustache γ)
         => ToMustache (α, β, γ) where
  toMustache (a, b, c) = toMustache [toMustache a, toMustache b, toMustache c]

instance (ToMustache α, ToMustache β, ToMustache γ, ToMustache δ)
         => ToMustache (α, β, γ, δ) where
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
         ) => ToMustache (α, β, γ, δ, ε) where
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
         ) => ToMustache (α, β, γ, δ, ε, ζ) where
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
         ) => ToMustache (α, β, γ, δ, ε, ζ, η) where
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
         ) => ToMustache (α, β, γ, δ, ε, ζ, η, θ) where
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

-- | A collection of templates with quick access via their hashed names
type TemplateCache = HM.HashMap String Template

-- | Type of key used for retrieving data from 'Value's
type Key = Text

{-|
  A compiled Template with metadata.
-}
data Template = Template
  { name     :: String
  , ast      :: STree
  , partials :: TemplateCache
  } deriving (Show)


deriveLift ''DataIdentifier
deriveLift ''Node
deriveLift ''Template

-- Data.HashMap 0.2.17.0 introduces its own Lift instance
#if !MIN_VERSION_unordered_containers(0,2,17)
instance Lift TemplateCache where
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped m = [|| HM.fromList $$(liftTyped $ HM.toList m) ||]
#else
  lift m = [| HM.fromList $(lift $ HM.toList m) |]
#endif
#endif

--Data.Text 1.2.4.0 introduces its own Lift Text instance
#if !MIN_VERSION_text(1,2,4)
instance Lift Text where
  lift = lift . unpack
#endif

