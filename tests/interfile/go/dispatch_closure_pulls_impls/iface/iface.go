package iface

// Processor is a bodiless interface declaration.  Its Process method
// is an FBDecl in the call graph.  During the topological fold the
// FBDecl case must merge implementation signatures into the empty
// skeleton; if the relevant subgraph lacks the impl, the merge has
// no inputs and the resulting signature is empty (unsound — callers
// would see "no taint propagation" instead of conservative fallback).
type Processor interface {
	Process(s string) string
}
