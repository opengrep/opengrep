module WhereGuardCase where

--ruleid: test-where-guard-case
viaWhere = sink v
  where v = source

--OK:
viaWhereOK = sink v
  where v = 42

guarded x
  --ruleid: test-where-guard-case
  | x > 0 = sink source
  --OK:
  | otherwise = sink 1

viaCase = case source of
  --ruleid: test-where-guard-case
  v -> sink v

--ruleid: test-where-guard-case
viaDollar = sink $ transform $ source

--ruleid: test-where-guard-case
viaParens = sink (transform source)

--OK:
sanitized = sink (sanitize source)

--OK:
sanitizedDollar = sink $ sanitize $ source
