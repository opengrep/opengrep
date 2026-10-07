class SwitchCase {
  void test(Integer mode) {
    final Integer CMD = 7;
    // ruleid: switch-case-constant-propagation-apex
    exec(CMD);
    switch on mode {
      when CMD {
        // ruleid: switch-case-constant-propagation-apex
        exec(CMD);
      }
    }
    // ruleid: switch-case-constant-propagation-apex
    exec(CMD);
  }

  enum Mode { A, B, RED }

  void laterCase(Mode mode) {
    String q = 'safe';
    switch on mode {
      when A { q = 'safe'; }
      when B {
        q = taint_source();
        // ruleid: taint-switch-case-apex
        sink(q);
      }
    }
  }

  void labelIsNotBound() {
    Mode y = taint_source();
    switch on y {
      when RED { }
    }
    // ok: taint-switch-case-apex
    sink(RED);
    // ruleid: taint-switch-case-apex
    sink(y);
  }

  void objectPatternStillBinds(SObject record) {
    switch on record {
      when Account account {
        account.Name = taint_source();
        // ruleid: taint-switch-case-apex
        sink(account.Name);
      }
    }
  }

  void twoLabels(Mode mode) {
    // ruleid: switch-case-apex-labels
    switch on mode {
      when A, B { sink('safe'); }
    }
  }

  void differentSecondLabel(Mode mode) {
    // ok: switch-case-apex-labels
    switch on mode {
      when A, RED { sink('safe'); }
    }
  }
}
