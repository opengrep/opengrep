module ImportQualified where

--ERROR:
import qualified Data.Map

import qualified Data.Text as T

--ERROR:
import Data.List

foo = 1
