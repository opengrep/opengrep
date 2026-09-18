package main

import "fmt"

func sink(data string) {
	fmt.Println(data)
}

func send_to_sink(data string) {
	// ruleid: test-taint-intrafile
	sink(data)
}

func send_to_sink_to_be_cleaned(data string) {
	// ok: test-taint-intrafile
	sink(data)
}
