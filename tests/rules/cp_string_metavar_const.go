package main

import "strings"

const sqlQueryGetCount = `SELECT COUNT(*) FROM #TABLE_NAME# #WHERE_CLAUSE#`

func direct(label string) string {
	// ruleid: cp-string-metavar-const
	sqlQuery := `SELECT COUNT(*) FROM #TABLE_NAME# #WHERE_CLAUSE#`
	sqlQuery = strings.Replace(sqlQuery, "#TABLE_NAME#", label, 1)
	return sqlQuery
}

func viaConst(label string) string {
	// ruleid: cp-string-metavar-const
	sqlQuery := sqlQueryGetCount
	sqlQuery = strings.Replace(sqlQuery, "#TABLE_NAME#", label, 1)
	return sqlQuery
}
