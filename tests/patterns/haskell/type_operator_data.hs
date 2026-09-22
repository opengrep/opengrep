module TypeOperatorData where

--ERROR:
data T = Int :+: Bool

data U = Int :*: Bool

data V = V Int Bool
