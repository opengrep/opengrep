//go:build !windows

package editor

func (e Editor) args(path string) []string {
	return []string{e.Shell, path}
}

func (e Editor) quote(path string) string {
	return "'" + e.Shell + "'"
}

func (e Editor) Launch(path string) error {
	args := e.args(path)
	// ruleid: build-constraint-alternative-files
	sink(args[1])
	// ok: build-constraint-alternative-files
	sink(e.quote(path))
	return nil
}
