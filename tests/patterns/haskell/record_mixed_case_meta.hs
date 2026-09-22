module RecordMixedCase where

--ERROR:
data User = MkUser { password :: String }

data Cfg = Cfg { port :: Int }
