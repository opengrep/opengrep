package util

type Store struct{}

func Run(q string) {
	// ok: package-by-import-path
	sink(q)
}
