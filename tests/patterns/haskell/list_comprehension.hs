module ListComp where

--ERROR:
evens = [x | x <- [1..10]]

--ERROR:
doubled = [x * 2 | x <- [1..5]]

noComp = [1, 2, 3]
