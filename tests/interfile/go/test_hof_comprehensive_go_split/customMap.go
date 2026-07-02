package main

func customMap(arr []string, callback func(string) string) []string {
	result := make([]string, 0)
	for _, item := range arr {
		result = append(result, callback(item))
	}
	return result
}
