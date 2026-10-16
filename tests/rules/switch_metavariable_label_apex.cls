class SwitchLabels {
  void enumLabel(Mode k) {
    // ruleid: switch-metavariable-label-apex
    switch on k {
      when FOO { a(); }
    }
  }

  void integerLabel(Integer k) {
    // ruleid: switch-metavariable-label-apex
    switch on k {
      when 2 { a(); }
    }
  }

  void objectLabel(SObject k) {
    // ruleid: switch-metavariable-label-apex
    switch on k {
      when Account acc { a(); }
    }
  }
}
