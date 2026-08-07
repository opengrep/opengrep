package main

// RealStore genuinely implements Store (same params, same return).
type RealStore struct{}

func (r *RealStore) Put(id int64, val string) error {
	// ruleid: dispatch-param-type-mismatch
	sink(val)
	return nil
}
