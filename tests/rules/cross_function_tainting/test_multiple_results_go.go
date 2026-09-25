package main

type DTO struct {
	UID      string
	FolderID int64
}

type Store interface {
	WithTx(fn func() error) error
}

type Service struct {
	store Store
}

func (s *Service) patch(uid string) (DTO, error) {
	var dto DTO
	err := s.store.WithTx(func() error {
		dto = DTO{UID: uid, FolderID: 0}
		return nil
	})
	return dto, err
}

type Query struct {
	ID *int64
}

func (s *Service) handler() {
	element, _ := s.patch(source())
	// ok: test-multiple-results-go
	sink(&Query{ID: &element.FolderID})
	// ruleid: test-multiple-results-go
	sink(element.UID)
}

type User struct {
	Name  string
	Token string
}

func load(token string) (User, error) {
	return User{Name: "x", Token: token}, nil
}

func direct() {
	u, _ := load(source())
	// ok: test-multiple-results-go
	sink(u.Name)
	// ruleid: test-multiple-results-go
	sink(u.Token)
}

func loadVia(token string) (User, error) {
	return load(token)
}

func passedOn() {
	u, _ := loadVia(source())
	// ok: test-multiple-results-go
	sink(u.Name)
	// ruleid: test-multiple-results-go
	sink(u.Token)
}
