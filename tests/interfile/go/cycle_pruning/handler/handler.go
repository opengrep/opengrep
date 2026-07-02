package handler

import (
	"context"
	"encoding/json"
	"net/http"

	"example.com/interfile-check/iface"
)

type Handler struct {
	store iface.Store
}

func NewHandler(store iface.Store) *Handler {
	return &Handler{store: store}
}

func (h *Handler) GetItem(w http.ResponseWriter, r *http.Request) {
	// SOURCE: user-controlled input from URL query parameter.
	uid := r.URL.Query().Get("uid")

	ctx := context.Background()
	// This calls Store.Get (the interface method).  Interfile taint
	// must propagate uid through the dispatch-merged signature into
	// realStore.Get, where it reaches the SQL query sink.
	item, err := h.store.Get(ctx, uid)
	if err != nil {
		http.Error(w, err.Error(), http.StatusNotFound)
		return
	}

	json.NewEncoder(w).Encode(item)
}
