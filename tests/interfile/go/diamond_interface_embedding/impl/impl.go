package impl

// DB is the sink-bearing back-end.  Impl below holds one and exposes
// M (which satisfies top.Top via the diamond-inherited base.Base.M).
type DB interface {
	Query(q string) string
}

type Impl struct {
	DB DB
}

func (i *Impl) M(s string) string {
	// ruleid: diamond-interface-embedding
	return i.DB.Query(s)
}
