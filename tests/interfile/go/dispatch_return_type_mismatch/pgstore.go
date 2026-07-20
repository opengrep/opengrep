package main

// PGStore genuinely implements Store (Fetch returns string). The
// dispatch edge Store.Fetch -> PGStore.Fetch must carry the caller's
// tainted argument into this sink.
type PGStore struct{}

func (p *PGStore) Fetch(q string) string {
	// ruleid: dispatch-return-type-mismatch
	sink(q)
	return q
}
