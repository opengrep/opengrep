package p

type Svc struct{ Z string }

func (s *Svc) init() error {
	if c() {
		return nil
	}
	if c() {
		return nil
	}
	if c() {
		return nil
	}
	if c() {
		return nil
	}
	s.Z = source()
	return nil
}

func initSvc(o *Svc) {
	if c() {
		return
	}
	if c() {
		return
	}
	if c() {
		return
	}
	o.Z = source()
}

func handler() {
	s := &Svc{}
	s.init()
	// ruleid: test-many-returns-go
	sink(s.Z)
	o := &Svc{}
	initSvc(o)
	// ruleid: test-many-returns-go
	sink(o.Z)
}
