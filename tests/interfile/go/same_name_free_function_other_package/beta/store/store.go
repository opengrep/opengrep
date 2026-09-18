package store

// A free function with the same simple name in a DIFFERENT directory but same package
// basename ("store").  Same name AND arity, no sink.  Only exists to
// collide in build_by_package, which keys on the dir basename.
func Save(db any, query string) string {
	return "noop: " + query
}
