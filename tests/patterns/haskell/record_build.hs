module RecBuild where


data User = User { userName :: String }

--ERROR:
u = User { userName = "bob" }
