package main

import (
	"net/http"

	"example.com/interfile-check/db"
	"example.com/interfile-check/handler"
	"example.com/interfile-check/impl"
)

func main() {
	database := &db.DB{}
	realStore := impl.NewRealStore(database)
	cachedStore := impl.NewCachedStore(realStore)
	h := handler.NewHandler(cachedStore)

	http.HandleFunc("/item", h.GetItem)
	http.ListenAndServe(":8080", nil)
}
