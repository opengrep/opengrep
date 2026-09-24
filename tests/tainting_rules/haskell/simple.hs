module Simple where

--ruleid: test-simple
direct = sink source

--OK:
nontainted = sink 1

viaLet =
  let x = source
  --ruleid: test-simple
  in sink x

--OK:
viaLetOK =
  let x = 42
  in sink x
