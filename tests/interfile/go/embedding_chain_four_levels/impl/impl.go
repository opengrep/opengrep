package impl

type DB interface {
	Query(q string) string
}

// Impl satisfies top.Top by implementing M.  The sink lives here.
// If the embedding chain doesn't propagate M up to Top, no Dispatch
// edge connects Top.M to Impl.M and this sink stays cold.
type Impl struct {
	DB DB
}

func (i *Impl) M(s string) string {
	// ruleid: embedding-chain-four-levels
	return i.DB.Query(s)
}
