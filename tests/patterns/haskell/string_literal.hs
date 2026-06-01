module StringLit where

--ERROR:
bad = checkAuth "password"

--ERROR:
worse = "password"

good = checkAuth token
