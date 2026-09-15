package pkga

import "net/http"

// Caller in the same package as the sink-bearing Service.Get.
// `s` is a bare identifier (*Service via &Service{...}); the call
// site s.Get(q) is the bare-Id receiver branch in identify_callee.
// pkgb.Service.Get has the same name and arity; without same-file/
// same-dir disambiguation the resolver returns None (ambiguous via
// pick_by_arity) and the edge is silently dropped.
func Handle(w http.ResponseWriter, r *http.Request, db Database) {
	s := &Service{DB: db}
	// SOURCE: user-controlled URL query param.
	q := r.URL.Query().Get("q")
	_ = s.Get(q)
}
