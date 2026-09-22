module CallChain where

transform :: String -> String
transform x = reverse x

flow =
  let a = tainted
      b = transform a
  --ruleid: test-call-chain
  in unsafeEval b

--OK:
clean =
  let a = "clean"
  in unsafeEval a
