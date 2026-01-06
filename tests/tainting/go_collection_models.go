// Test: Go collection models for taint propagation
// Note: Go uses map indexing m[k]=v, not methods
// Only sync.Map has Store/Load methods

package main

import (
	"fmt"
	"sync"
)

func testSyncMapStore(tainted string) {
	var m sync.Map
	m.Store("key", tainted)
	value, _ := m.Load("key")
	// ruleid: go-collection-taint
	sink(value)
}

func sink(data interface{}) {
	fmt.Println(data)
}
