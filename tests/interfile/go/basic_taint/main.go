package main

func vulnerable() {
	data := get_tainted()
	send_to_sink(data)
}

func safe() {
	data := get_tainted()
	cleaned := clean(data)
	send_to_sink_to_be_cleaned(cleaned)
}

func main() {
	vulnerable()
	safe()
}
