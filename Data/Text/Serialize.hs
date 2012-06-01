module Data.Text.Serialize(
  -- * Show
  Show(..),
  showLazyText,
  -- ** Functions for custom 'Show' instances
  showParen,
  buildPrec,
  preludeShowPrec,
  defaultShowPrefix,
  -- * Read
  Read(..),
  read,
  readEither,
  -- * Precedences
  appPrec,
  ) where

import qualified Prelude
import Data.Text.Serialize.Show
import Data.Text.Serialize.Read
