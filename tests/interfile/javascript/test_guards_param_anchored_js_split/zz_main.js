// Parameter-anchored branch guards via aliased object literal at the
// call site. The caller binds a literal [{...}] to a local, and the
// callee branches on a field of the parameter. At instantiation, the
// svalue walker must step through the local's [id_svalue] (a [G.Sym]
// of a [G.Container Dict]) to recover the literal at the branch path.





// ---------- Direct field truthiness (Bool literal at call site) ----------







// ---------- Field equality cond (Operator(Eq, [Fetch opts.code; Lit N])) ----------







// ---------- Nested field, two levels ----------





