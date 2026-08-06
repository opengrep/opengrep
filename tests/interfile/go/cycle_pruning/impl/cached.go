package impl

import (
	"context"
	"sync"

	"example.com/interfile-check/iface"
)

type cachedStore struct {
	// inner is typed as iface.Store (the interface).  Calling
	// s.inner.Get creates a Call edge from Store.Get to
	// cachedStore.Get in the call graph, forming a direct cycle
	// with the Dispatch edge cachedStore.Get -> Store.Get.
	inner iface.Store
	mu    sync.RWMutex
	cache map[string]iface.Item
}

func NewCachedStore(inner iface.Store) iface.Store {
	return &cachedStore{
		inner: inner,
		cache: make(map[string]iface.Item),
	}
}

func (s *cachedStore) Get(ctx context.Context, uid string) (iface.Item, error) {
	s.mu.RLock()
	if item, ok := s.cache[uid]; ok {
		s.mu.RUnlock()
		return item, nil
	}
	s.mu.RUnlock()

	// This call targets the Store interface, not a concrete
	// implementation.  It is the edge that creates the cycle.
	item, err := s.inner.Get(ctx, uid)
	if err != nil {
		return iface.Item{}, err
	}

	s.mu.Lock()
	s.cache[uid] = item
	s.mu.Unlock()
	return item, nil
}
