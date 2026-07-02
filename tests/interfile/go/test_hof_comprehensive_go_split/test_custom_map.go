package main

func test_custom_map() {
	arr := []string{source()}
	customMap(arr, func(x string) string {
		// ruleid: test-hof-taint
		sink(x)
		return x
	})
}
