module QualifiedCall where

import qualified Data.Text

--ERROR:
a = Data.Text.unpack someText

--ERROR:
b = Data.Text.pack "hello"

noMatchDifferentModule = Data.List.sort xs
noMatchNotQualified = unpack someText
