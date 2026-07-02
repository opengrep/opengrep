package main

func customForEach(arr []string, callback func(string)) {
	for _, item := range arr {
		callback(item)
	}
}
