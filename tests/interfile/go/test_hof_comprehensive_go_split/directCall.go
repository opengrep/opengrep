package main

func directCall(callback func(string)) {
	callback(source())
}
