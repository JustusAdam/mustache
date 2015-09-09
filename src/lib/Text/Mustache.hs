{-# LANGUAGE LambdaCase #-}
module Text.Mustache
  (
  -- * Compiling

  -- ** Automatic
    compileTemplate

  -- ** Manually
  , compileTemplateWithCache, parseTemplate

  -- * Rendering

  -- ** Generic

  , substitute

  -- ** Specialized

  , substituteValue

  -- * Data structures
  , MustacheTemplate(..), ToMustache, toMustache

  -- * Util

  -- | These are functions used internally by the parser and renderer. Whether
  -- these  will continue to be exposed is to be seen.
  , getFile , getPartials , getPartials', toString, search, Context(..)
  ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Bool
import           Data.Foldable              (fold)
import           Data.HashMap.Strict        as HM hiding (map)
import           Data.List
import           Data.Monoid
import           Data.Text                  hiding (concat, find, map, uncons)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Traversable           (traverse)
import qualified Data.Vector                as V
import           System.Directory
import           System.FilePath
import           Text.HTML.TagSoup          (escapeHTML)
import           Text.Mustache.AST
import           Text.Mustache.Internal
import           Text.Mustache.Parser
import           Text.Parsec.Error
import           Text.Parsec.Pos
import           Text.Printf
