package svc

import "example.com/embedclause/internal/httpclient"

type Outer struct {
	client.Base
}

func Run(q string) {
	var o Outer
	o.Query(q)
}
