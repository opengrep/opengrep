package editor

func (e Editor) args(path string) []string {
	return []string{"cmd", "/C", e.Shell}
}

func (e Editor) quote(path string) string {
	return "\"" + path + "\""
}

func (e Editor) Launch(path string) error {
	args := e.args(path)
	// ok: build-constraint-alternative-files
	sink(args[2])
	// ruleid: build-constraint-alternative-files
	sink(e.quote(path))
	return nil
}
