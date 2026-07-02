package pkgb

// Homonym: same struct name, same method name, same arity as pkga.Service.Get.
// No sink here — included to force the disambiguation pipeline to choose
// pkga over pkgb.  Without the same-dir / same-file filters, pick_by_arity
// sees two candidates and returns None, dropping the edge.
type Service struct{}

func (s *Service) Get(query string) string {
	return "noop: " + query
}
