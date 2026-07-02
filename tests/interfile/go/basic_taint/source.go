package main

func source() string {
	return "tainted_data"
}

func get_tainted() string {
	return source()
}
