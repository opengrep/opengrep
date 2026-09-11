package main

func test_custom_foreach() {
	arr := []string{source()}
	customForEach(arr, func(x string) {
		// ruleid: test-hof-taint
		sink(x)
	})
}
