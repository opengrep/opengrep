# Opengrep reference

This reference covers the command line and the rule format of opengrep.

- [Commands](commands.md): `opengrep scan`, `opengrep test` and the other subcommands.
- [Flags](flags.md): every command-line flag, with the commands that accept it.
- [Rule syntax](rule-syntax.md): the keys of a rule.
- [Rule options](rule-options.md): the keys of a rule's `options:` block.
- [Environment variables](environment.md): the variables opengrep reads.
- [Internal and debugging interfaces](internal.md): interfaces meant for opengrep's
  own development, listed but not documented in detail.

## Further reading

The [Opengrep wiki](https://github.com/opengrep/opengrep/wiki) on GitHub goes
deeper into parts of the taint analysis than this reference does. Its pages are
tutorials with worked examples, and notes on how the analysis works inside:

- [Intrafile taint tracking](https://github.com/opengrep/opengrep/wiki/Intrafile-tainting-tutorial):
  following tainted data from function to function within one file, with
  `--taint-intrafile`.
- [Taint tracking in higher-order functions](https://github.com/opengrep/opengrep/wiki/Higher-order-functions-tutorial):
  taint carried through callbacks such as `map` and `forEach`.
- [Built-in methods that taint](https://github.com/opengrep/opengrep/wiki/Methods-that-taint):
  the standard library methods, such as `list.add` and `map.get`, that carry
  taint into and out of a collection, language by language.
- [Guarded taint signatures](https://github.com/opengrep/opengrep/wiki/Guarded-taint-signatures):
  an experimental refinement that drops a finding when the condition leading to
  the sink can never hold for a call.
