// A struct literal returned by a function keeps its fields apart: the field
// that holds a clean argument carries no taint at the caller.
package main

type DTO struct {
	UID      string
	FolderID int64
}

func one(uid string, folder int64) DTO {
	return DTO{UID: uid, FolderID: folder}
}

func two(uid string, folder int64) (DTO, error) {
	return DTO{UID: uid, FolderID: folder}, nil
}

func constant(uid string) DTO {
	return DTO{UID: uid, FolderID: 3}
}

func returned() {
	e := one(source(), 3)
	// ruleid: composite_return_field_go
	sink(e.UID)
	// todook: composite_return_field_go
	sink(e.FolderID)
}

func returnedWithError() {
	e, _ := two(source(), 3)
	// ruleid: composite_return_field_go
	sink(e.UID)
	// todook: composite_return_field_go
	sink(e.FolderID)
}

func returnedConstant() {
	e := constant(source())
	// ruleid: composite_return_field_go
	sink(e.UID)
	// ok: composite_return_field_go
	sink(e.FolderID)
}

func local() {
	e := DTO{UID: source(), FolderID: 3}
	// ruleid: composite_return_field_go
	sink(e.UID)
	// ok: composite_return_field_go
	sink(e.FolderID)
}

func readLiteral(d DTO) {
	// ok: composite_return_field_go
	sink(d.FolderID)
}

func passed() {
	readLiteral(DTO{UID: source(), FolderID: 3})
}

func readReturned(d DTO) {
	// todook: composite_return_field_go
	sink(d.FolderID)
}

func passedReturned() {
	readReturned(one(source(), 3))
}

type H struct{ d DTO }

func stored(h *H) {
	h.d = one(source(), 3)
	// ruleid: composite_return_field_go
	sink(h.d.UID)
	// todook: composite_return_field_go
	sink(h.d.FolderID)
}

func captured() {
	var e DTO
	f := func() { e = one(source(), 3) }
	f()
	// ruleid: composite_return_field_go
	sink(e.UID)
	// todook: composite_return_field_go
	sink(e.FolderID)
}

func partial(uid string) DTO {
	return DTO{UID: uid}
}

func returnedPartial() {
	e := partial(source())
	// ruleid: composite_return_field_go
	sink(e.UID)
	// todook: composite_return_field_go
	sink(e.FolderID)
}

func localPartial() {
	e := DTO{UID: source()}
	// ruleid: composite_return_field_go
	sink(e.UID)
	// todook: composite_return_field_go
	sink(e.FolderID)
}
