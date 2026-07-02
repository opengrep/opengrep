package iface

import "context"

type Item struct {
	UID  string
	Name string
}

// Store is the interface whose dispatch merge is the focus of this test.
//
// Two implementations exist: realStore (impl/real.go) and cachedStore
// (impl/cached.go).  cachedStore.Get delegates to Store.Get (the
// interface method), creating a direct cycle in the SCIP call graph:
//
//   cachedStore.Get --Dispatch--> Store.Get  (impl -> interface)
//   Store.Get       --Call------> cachedStore.Get  (callee -> caller)
//
// Before the fix, the topo sort might process Store.Get before
// realStore.Get.  At that point no implementation signatures are
// available, so Store.Get gets an empty dispatch-merged signature.
// Handler.GetItem calls Store.Get, sees "no effects", and the taint
// from the URL parameter never reaches the SQL sink in realStore.Get.
//
// After the fix (prune_impl_interface_cycles), the Call edge
// Store.Get -> cachedStore.Get is removed from the relevant graph.
// The topo sort processes realStore.Get and cachedStore.Get before
// Store.Get.  Dispatch merge now has both implementations' signatures,
// so taint propagates through the interface correctly.
type Store interface {
	Get(ctx context.Context, uid string) (Item, error)
}
