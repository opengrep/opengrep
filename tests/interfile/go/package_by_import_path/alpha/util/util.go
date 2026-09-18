package util

type Store struct{}

func Run(q string) {
	// ruleid: package-by-import-path
	sink(q)
}
