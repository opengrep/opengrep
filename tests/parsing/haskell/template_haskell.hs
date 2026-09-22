{-# LANGUAGE TemplateHaskell #-}
module TemplateHaskell where

import Language.Haskell.TH

-- Expression quote
exprQuote :: Q Exp
exprQuote = [| 1 + 2 |]

-- Type quote
typeQuote :: Q Type
typeQuote = [t| Int -> Bool |]

-- Declaration quote
declQuote :: Q [Dec]
declQuote = [d| myId x = x |]

-- Splice (calling a TH function)
-- $(makeLenses ''Person)  -- would require lens library

-- Name quote
intName :: Name
intName = ''Int

funName :: Name
funName = 'show
