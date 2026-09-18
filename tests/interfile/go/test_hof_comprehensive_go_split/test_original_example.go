package main

func test_original_example() {
	history := getHistory("name", "owner")
	customForEach([]string{history}, func(node string) {
		changes := node
		// ruleid: test-hof-taint
		sink(changes)
	})
}
