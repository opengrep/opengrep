package main

// The taint source is injected INSIDE the cross-file cycle (q returns
// r(source())), so it must flow around the interfile cycle to become p's
// return value.
func q(x string) string {
	return r(source())
}
