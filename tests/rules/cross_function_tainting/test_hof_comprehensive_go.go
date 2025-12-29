// Comprehensive HOF test for Go: Custom higher-order functions
// All of these should detect taint flow from source() to sink()

package main

// ===== Custom HOF Functions =====

func customMap(arr []string, callback func(string) string) []string {
	result := make([]string, 0)
	for _, item := range arr {
		result = append(result, callback(item))
	}
	return result
}

func customForEach(arr []string, callback func(string)) {
	for _, item := range arr {
		callback(item)
	}
}

func directCall(callback func(string)) {
	callback(source())
}

// ===== Test Cases =====

func test_custom_map() {
	arr := []string{source()}
	customMap(arr, func(x string) string {
		// ruleid: test-hof-taint
		sink(x)
		return x
	})
}

func test_custom_foreach() {
	arr := []string{source()}
	customForEach(arr, func(x string) {
		// ruleid: test-hof-taint
		sink(x)
	})
}

func test_direct_call() {
	directCall(func(x string) {
		// ruleid: test-hof-taint
		sink(x)
	})
}

// ===== Complex Example =====

func getHistory(name string, owner string) string {
	result := source()
	return result
}

func test_original_example() {
	history := getHistory("name", "owner")
	customForEach([]string{history}, func(node string) {
		changes := node
		// ruleid: test-hof-taint
		sink(changes)
	})
}

// Stub functions
func source() string { return "tainted" }
func sink(s string)  {}

func main() {
	test_custom_map()
	test_custom_foreach()
	test_direct_call()
	test_original_example()
}

// ===== Top-level HOF Tests =====
// These test HOF callback detection at package level

func toplevelHandler(x string) {
	// ruleid: test-hof-taint
	sink(x)
}

// Package-level lambda callback (like Python's module-level lambda)
// ruleid: test-hof-taint
var toplevelSink = func(x string) { sink(x) }

func init() {
	// Call package-level lambda with tainted data
	toplevelSink(source())

	// Top-level user-defined HOF with named callback
	toplevelItems := []string{source()}
	customForEach(toplevelItems, toplevelHandler)
}
