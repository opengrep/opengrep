# Opengrep: The Only SAST Engine That Supports Brainfuck

**We did it. We added Brainfuck to Opengrep. No, we're not sorry.**

While other SAST tools are still trying to catch up on Go support, we leapfrogged the entire industry and asked the question nobody else had the courage to ask: *what about the developers writing production code in Brainfuck?*

Today, Opengrep becomes the world's first (and only) static analysis engine with full Brainfuck support. Pattern matching. Comment-aware parsing. Nested bracket analysis that regex can never achieve. We expect Snyk to announce their Brainfuck beta sometime in 2028.

---

## Quick Start

**Install**: You already have it. Just update Opengrep.

**Write a rule** (`rules.yaml`):
```yaml
rules:
  - id: echo-loop
    pattern: "[,.]"
    languages: [brainfuck]
    message: Echo loop detected — reads input and writes to output in a loop
    severity: WARNING
```

**Scan your Brainfuck codebase**:
```
$ opengrep --config rules.yaml my_app.bf
```

---

## Why This Matters (It Doesn't)

### 1. Comment-Aware Parsing

In Brainfuck, everything that isn't `><+-.,[]` is a comment. That means this:

```brainfuck
[,.]
```

And this:

```brainfuck
[ read a byte , then print it . ]
```

Are the **same program**. A regex sees two completely different strings. Opengrep sees the same AST. One rule catches both.

### 2. Nested Bracket Analysis

Find any loop containing another loop:

```yaml
rules:
  - id: nested-loop
    pattern: "[...[...]...]"
    languages: [brainfuck]
    message: Nested loop detected
    severity: INFO
```

This matches all of these:

```brainfuck
[[-]]
[ clear the cell [-] then end ]
[>++[>+<-]<-]
```

### 3. Real Patterns on Real Programs

Here's a rot13 implementation:

```brainfuck
,+[-->++++[>++++++++<-]<[->+>-[>+>>]>[+[-<+>]>+>>]<<<<<<]>>[-]>>--[-[>
-<[-]]]>+[-<+++++++++++++<[->-[>+>>]>[+[-<+>]>+>>]<<<<<]>[-]>[-]+>[<--
>-[<+>-]]<[<<<<+++++++++++++>>>>-]]<<[-]<<+.[-]<,+]
```

Find every output instruction:

```
$ opengrep -e '.' -l brainfuck rot13.bf

rot13.bf
2┆ ...+.[-]<,+]
```

Found it — the single `.` buried inside three levels of nested loops.

---

## How It Works

Through mass amounts of dark magic that we shall not discuss in polite company. Loops become proper nested structures. Comments vanish. What remains is pure intent — ready for pattern matching, structural queries, and existential dread.

---

## FAQ

**Q: Can't I just use regex for this?**
A: Sure. And you can also drive a nail with a shoe. But are you really going to put `\[.*\[.*\].*\]` in your CI pipeline and tell your security team it's covered?

**Q: Should I use this in production?**
A: If you're writing Brainfuck in production, SAST is the least of your concerns.

**Q: Does it support taint analysis?**
A: Brainfuck has no variables, so taint tracking would require modeling the tape pointer. We'll get to it right after we finish the Brainfuck type checker.

**Q: Why?**
A: Because we could. And because it took less time than writing this announcement.

---

*Opengrep — securing code in every language. Even that one.*
