//go:build integration

package suite

func source() string { return "tainted" }

func Run() {
	store(source())
}
