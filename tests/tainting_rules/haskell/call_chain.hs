module CallChain where

transform x = reverse x

--ruleid: test-call-chain
flow =
  let a = tainted
      b = transform a
  in unsafeEval b

--OK:
clean =
  let a = "clean"
  in unsafeEval a
