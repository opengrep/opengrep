package impl

type DB interface {
	Query(query string) string
}

type ClientImpl struct {
	DB DB
}

// Authenticate implements both auth.Authenticator and (via embedding)
// client.Client's Authenticate.  The SINK lives here; dispatch must
// route from Client.Authenticate's FBDecl down to this impl.
func (c *ClientImpl) Authenticate(token string) string {
	// ruleid: interface-embedding-dispatch
	return c.DB.Query(token)
}

func (c *ClientImpl) Name() string     { return "impl" }
func (c *ClientImpl) IsEnabled() bool  { return true }
