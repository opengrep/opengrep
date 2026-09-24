module RecordFieldMeta where

--ERROR:
data User = User { password :: String }

data Other = User { count :: Int }
