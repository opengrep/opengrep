let CMD = 7
let FIRST = 1
let SECOND = 2
let RED = 3

func test(_ mode: Int) {
  var value = 0
  // ruleid: switch-case-constant-propagation-swift
  exec(CMD)
  switch mode {
  case CMD:
    // ruleid: switch-case-constant-propagation-swift
    exec(CMD)
  case SECOND:
    value = taint_source()
    // ruleid: taint-switch-case-swift
    sink(value)
  default:
    value = 0
  }
  // ruleid: switch-case-constant-propagation-swift
  exec(CMD)
  // ruleid: taint-switch-case-swift
  sink(value)
}

func explicitShadowing(_ mode: Int, _ CMD: Int) {
  switch mode {
  case CMD:
    // ok: switch-case-constant-propagation-swift
    exec(CMD)
  default:
    break
  }
}

func tupleDeclarationShadowing() {
  let (CMD, _) = (1, 0)
  // ok: switch-case-constant-propagation-swift
  exec(CMD)
}

func mixedTuplePattern(_ mode: (Int, Int)) {
  switch mode {
  case (CMD, let bound):
    // ruleid: switch-case-constant-propagation-swift
    exec(CMD)
    // ok: switch-case-constant-propagation-swift
    exec(bound)
  default:
    break
  }
}

func caseReadsExistingTaint(_ mode: Int) {
  let value = taint_source()
  switch mode {
  case value:
    // ruleid: taint-switch-case-swift
    sink(value)
  default:
    break
  }
  // ruleid: taint-switch-case-swift
  sink(value)
}

func labelDoesNotBind() {
  let mode = taint_source()
  switch mode {
  case RED:
    break
  default:
    break
  }
  // ok: taint-switch-case-swift
  sink(RED)
}

func tupleLabelDoesNotBind() {
  let mode = (taint_source(), 0)
  switch mode {
  case (RED, _):
    break
  default:
    break
  }
  // ok: taint-switch-case-swift
  sink(RED)
}

func explicitBindingCarriesTaint() {
  let mode = taint_source()
  // ruleid: switch-case-swift-binding
  switch mode {
  case let bound:
    // ruleid: taint-switch-case-swift
    sink(bound)
  }
}

func explicitTupleBindingCarriesTaint() {
  let mode = (taint_source(), 0)
  switch mode {
  case let (CMD, second):
    // ok: switch-case-constant-propagation-swift
    exec(CMD)
    // ruleid: taint-switch-case-swift
    sink(CMD)
    // ok: taint-switch-case-swift
    sink(second)
  }
}

func explicitFallthrough(_ mode: Int) {
  var value = 0
  switch mode {
  case FIRST:
    value = taint_source()
    fallthrough
  case SECOND:
    // ruleid: taint-switch-case-swift
    sink(value)
  default:
    break
  }
}

enum Choice {
  case first, second
}

func enumLaterCase(_ mode: Choice) {
  switch mode {
  case .first:
    break
  case .second:
    // ruleid: taint-switch-case-swift
    sink(taint_source())
  }
}

func enumLabelDoesNotBind() {
  let mode: Choice = taint_choice_source()
  switch mode {
  case Choice.first:
    break
  default:
    break
  }
  // ok: taint-switch-case-swift
  sink(Choice.first)
}
