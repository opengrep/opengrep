module InstanceBodyEllipsis where

--ERROR:
instance Show Color where
  show Red = "red"
  show _ = "other"

--ERROR:
instance Show Point where
  show (Point x y) = show (x, y)

instance Eq Color where
  a == b = True
