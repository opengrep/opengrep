package main

// Cache.Put has Store.Put's name, arity AND return type (error), but its
// second param is int, not string — so Cache does NOT implement Store.
// Return-type matching alone can't tell them apart; parameter-type
// matching rejects it, so this sink must not fire from a Store.Put call.
type Cache struct{}

func (c *Cache) Put(id int64, code int) error {
	// ok: dispatch-param-type-mismatch
	sink(code)
	return nil
}
