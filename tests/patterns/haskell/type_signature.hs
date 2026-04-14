module TypeSig where

--ERROR:
annotatedInt = (42 :: Int)

--ERROR:
annotatedFunc = (myFunc :: Int -> Bool)

--ERROR:
annotatedStr = ("hello" :: String)

noAnnotation = 42
