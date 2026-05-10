module Simple where

--ruleid: test-simple
direct = sink source

--OK:
nontainted = sink 1

--ruleid: test-simple
viaLet =
  let x = source
  in sink x

--OK:
viaLetOK =
  let x = 42
  in sink x
