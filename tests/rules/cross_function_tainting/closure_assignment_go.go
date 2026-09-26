package m

type DTO struct {
	UID      string
	FolderID int
}

func one(uid string, folder int) DTO {
	return DTO{UID: uid, FolderID: folder}
}

func run(f func()) {
	f()
}

func scalarCalledByVariable() {
	var s string
	f := func() { s = source() }
	f()
	// ruleid: closure_assignment_go
	sink(s)
}

func scalarCalledImmediately() {
	var s string
	func() { s = source() }()
	// ruleid: closure_assignment_go
	sink(s)
}

func scalarPassedToCall() {
	var s string
	run(func() { s = source() })
	// ruleid: closure_assignment_go
	sink(s)
}

func literalCalledByVariable() {
	var e DTO
	f := func() { e = DTO{UID: source(), FolderID: 3} }
	f()
	// ruleid: closure_assignment_go
	sink(e.UID)
	// todook: closure_assignment_go
	sink(e.FolderID)
}

func returnedCalledByVariable() {
	var e DTO
	f := func() { e = one(source(), 3) }
	f()
	// ruleid: closure_assignment_go
	sink(e.UID)
	// todook: closure_assignment_go
	sink(e.FolderID)
}

func returnedCalledImmediately() {
	var e DTO
	func() { e = one(source(), 3) }()
	// ruleid: closure_assignment_go
	sink(e.UID)
	// todook: closure_assignment_go
	sink(e.FolderID)
}

func returnedPassedToCall() {
	var e DTO
	run(func() { e = one(source(), 3) })
	// ruleid: closure_assignment_go
	sink(e.UID)
	// todook: closure_assignment_go
	sink(e.FolderID)
}

func notCalled() {
	var s string
	f := func() { s = source() }
	_ = f
	// ok: closure_assignment_go
	sink(s)
}

func scalarReadAndWritten() {
	var s string
	f := func() {
		_ = s
		s = source()
	}
	f()
	// ruleid: closure_assignment_go
	sink(s)
}

func returnedReadAndWritten() {
	var e DTO
	f := func() {
		_ = e
		e = one(source(), 3)
	}
	f()
	// ruleid: closure_assignment_go
	sink(e.UID)
	// todook: closure_assignment_go
	sink(e.FolderID)
}
