module RecUpdate where


data User = User { userName :: String }
--ERROR:
u1 = User { userName = "bob" }

--ERROR:
u2 = u1 { userName = "alice" }
