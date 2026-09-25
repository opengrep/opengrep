package editor

func source() string { return "tainted" }

type Editor struct {
	Shell string
}

func (e Editor) LaunchTempFile(path string) error {
	return e.Launch(path)
}

func Run() {
	e := Editor{Shell: "sh"}
	e.LaunchTempFile(source())
}
