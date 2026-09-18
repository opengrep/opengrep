package main

// Cache.Fetch has the same name and arity as Store.Fetch but returns
// int, not string — so Cache does NOT implement Store (Go requires an
// exact return-type match). Name+arity-only matching wrongly wired it
// as a Store impl and merged this sink into Store.Fetch, producing a
// false positive here. Return-type matching rejects it.
type Cache struct{}

func (c *Cache) Fetch(key string) int {
	// ok: dispatch-return-type-mismatch
	sink(key)
	return 0
}
