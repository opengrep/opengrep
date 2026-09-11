package auth

// Authenticator declares the Authenticate method.  Client (below)
// embeds this interface so its Authenticate is reachable as
// Client.Authenticate via embedding inheritance.
type Authenticator interface {
	Authenticate(token string) string
}
