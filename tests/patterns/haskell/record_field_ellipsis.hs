module RecordFieldEllipsis where

--ERROR:
data User = User { uid :: Int, password :: String, age :: Int }

--ERROR:
data Cfg = Cfg { host :: String }

data Pt = Pt { px :: Int, py :: Int }
