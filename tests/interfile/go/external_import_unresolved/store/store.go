package store

func Save(q string) string {
	// ok: external-import-unresolved
	sink(q)
	return q
}
