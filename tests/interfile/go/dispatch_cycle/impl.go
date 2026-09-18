package main

type Impl struct{}

// Impl.Handle -> relay -> Handler.Handle -(dispatch)-> Impl.Handle : an INDIRECT
// impl<->interface cycle (prune only breaks DIRECT ones).  The taint SOURCE is
// injected INSIDE the cycle (relay passes source() through the interface), so
// Impl.Handle's return carries the taint only once the dispatched signature —
// merged from the impl that is itself in the cycle — converges to a fixpoint.
func (i Impl) Handle(x string) string {
	if len(x) > 100 {
		return x
	}
	return relay(x)
}

func relay(x string) string {
	var h Handler = Impl{}
	return h.Handle(source())
}
