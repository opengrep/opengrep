package impl

// DB is the back-end that performs the sink call.
type DB interface {
	Query(q string) string
}

// ProcessorImpl is the only concrete implementation of
// iface.Processor in the project.  It is constructed (and assigned
// to an interface-typed variable) in main.go; no Call edge directly
// reaches `(*ProcessorImpl).Process` — only the Dispatch edge from
// iface.Processor.Process does.  The relevant subgraph BFS
// therefore needs the dispatch-closure step to pull this function
// in.
type ProcessorImpl struct {
	DB DB
}

func (p *ProcessorImpl) Process(s string) string {
	// ruleid: dispatch-closure-pulls-impls
	return p.DB.Query(s)
}
