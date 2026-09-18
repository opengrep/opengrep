package pkga

// Service in pkga: its Get method is the SINK target.
// A Service.Get with the same simple name and the same arity exists in pkgb, with no sink.
type Service struct {
	DB Database
}

type Database interface {
	Query(query string) string
}

func (s *Service) Get(query string) string {
	// ruleid: same-name-across-packages
	return s.DB.Query(query)
}
