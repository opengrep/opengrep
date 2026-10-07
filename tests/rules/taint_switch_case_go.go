package p

const CMD = 7

func test(mode int) {
	q := ""
	// ruleid: switch-case-constant-propagation-go
	exec(CMD)
	switch mode {
	case CMD:
		// ruleid: switch-case-constant-propagation-go
		exec(CMD)
	case B:
		q = taint_source()
		// ruleid: taint-switch-case-go
		sink(q)
	}
	// ruleid: switch-case-constant-propagation-go
	exec(CMD)
}

func bareNameCaseIsNotABindingTarget() {
	y := taint_source()
	switch y {
	case RED:
	}
	// ok: taint-switch-case-go
	sink(RED)
}

func conditionOnlySwitch() {
	flag := taint_source()
	switch {
	case flag:
		// ruleid: taint-switch-case-go
		sink(taint_source())
		// ruleid: taint-switch-case-go
		sink(flag)
	}
}

func typeSwitchKeepsLaterCases(mode interface{}) {
	switch mode.(type) {
	case int:
	case string:
		// ruleid: taint-switch-case-go
		sink(taint_source())
	}
}

func explicitFallthrough(mode int) {
	value := "safe"
	switch mode {
	case A:
		value = taint_source()
		fallthrough
	case B:
		// ruleid: taint-switch-case-go
		sink(value)
	}
}
