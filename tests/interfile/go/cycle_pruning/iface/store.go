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
// interface method), creating a direct cycle in the call graph:
//
//   cachedStore.Get --Dispatch--> Store.Get  (impl -> interface)
//   Store.Get       --Call------> cachedStore.Get  (callee -> caller)
//
// Without pruning, the topo sort might process Store.Get before
// realStore.Get.  At that point no implementation signatures are
// available, so Store.Get gets an empty dispatch-merged signature.
// Handler.GetItem calls Store.Get, sees "no effects", and the taint
// from the URL parameter never reaches the SQL sink in realStore.Get.
//
// prune_impl_interface_cycles removes the Call edge
// Store.Get -> cachedStore.Get from the relevant graph, so the topo
// sort processes realStore.Get and cachedStore.Get before Store.Get.
// Dispatch merge then has both implementations' signatures and taint
// propagates through the interface correctly.
type Store interface {
	Get(ctx context.Context, uid string) (Item, error)
}
