package db

import "context"

type DB struct{}

func (d *DB) Query(ctx context.Context, query string) ([]map[string]interface{}, error) {
	return nil, nil
}
