package main

func init() {
	// Call package-level lambda with tainted data
	toplevelSink(source())

	// Top-level user-defined HOF with named callback
	toplevelItems := []string{source()}
	customForEach(toplevelItems, toplevelHandler)
}
