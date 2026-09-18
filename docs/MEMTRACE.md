# Memtrace

## Run

Run `MEMTRACE=<trace file> MEMTRACE_RATE=1e-4 ./bin/opengrep scan ...`. `1e-4` is the lowest rate memprof-limits allows.

## Read

The tools need their own switch on OCaml 5.1.1 with the packages `memtrace` and `memtrace_viewer`. Run `opam exec --switch=<that switch> -- memtrace-viewer -port <port> <trace file>`, which listens after loading the whole trace. Run `memtrace_hotspots <trace file>` for allocation by site.
