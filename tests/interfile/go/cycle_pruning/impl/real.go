package impl

import (
	"context"
	"fmt"

	"example.com/interfile-check/db"
	"example.com/interfile-check/iface"
)

type realStore struct {
	database *db.DB
}

func NewRealStore(database *db.DB) iface.Store {
	return &realStore{database: database}
}

func (s *realStore) Get(ctx context.Context, uid string) (iface.Item, error) {
	// SINK: uid is interpolated into a SQL query without sanitisation.
	// Taint from the URL parameter in handler.go must reach here via
	// the Store interface dispatch for the finding to be reported.
	query := fmt.Sprintf("SELECT * FROM items WHERE uid = '%s'", uid)
	// ruleid: sql-injection-via-interface-dispatch
	rows, err := s.database.Query(ctx, query)
	if err != nil {
		return iface.Item{}, err
	}
	if len(rows) == 0 {
		return iface.Item{}, fmt.Errorf("not found")
	}
	return iface.Item{UID: uid, Name: rows[0]["name"].(string)}, nil
}
