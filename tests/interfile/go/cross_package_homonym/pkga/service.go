package pkga

// Service in pkga: its Get method is the SINK target.
// A homonym Service.Get exists in pkgb with the same arity but no sink.
type Service struct {
	DB Database
}

type Database interface {
	Query(query string) string
}

func (s *Service) Get(query string) string {
	// ruleid: cross-package-homonym
	return s.DB.Query(query)
}
