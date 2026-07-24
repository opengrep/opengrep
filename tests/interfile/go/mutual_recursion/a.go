package main

// p, q, r are mutually recursive but live in SEPARATE files, so the cycle
// p -> q -> r -> p spans files -- a genuinely INTERFILE cycle, exercising the
// Interfile_dispatch signature fixpoint (not the intrafile one).
func p(x string) string {
	return q(x)
}

func source() string {
	return ""
}

func sink(s string) {}

func cond() bool {
	return false
}
