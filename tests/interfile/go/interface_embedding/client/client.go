package client

import "example.com/embedding-check/auth"

// Client embeds auth.Authenticator.  The embed parses as a Spread
// expression field, not a FuncDef, so a naive walk of Client would
// only see Name/IsEnabled.  Authenticate must be brought in via the
// project-wide embedding inheritance pass.
type Client interface {
	auth.Authenticator
	Name() string
	IsEnabled() bool
}
