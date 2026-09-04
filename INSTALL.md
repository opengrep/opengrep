# Install the Opengrep CLI with our install script

```sh
curl -fsSL https://raw.githubusercontent.com/opengrep/opengrep/main/install.sh | bash
```

- This will install Opengrep to `~/.opengrep/cli/<version>` and set up a `latest` symlink.
- To install a specific version, use:
  ```sh
  curl -fsSL https://raw.githubusercontent.com/opengrep/opengrep/main/install.sh | bash -s -- -v v1.4.0
  ```
- To list available versions:
  ```sh
  curl -fsSL https://raw.githubusercontent.com/opengrep/opengrep/main/install.sh | bash -s -- -l
  ```

# Install a release binary by hand

The binaries of each release are on the
[releases page](https://github.com/opengrep/opengrep/releases). Download the
asset for your platform, make it executable and put it where you want it. On
Windows, the archive holds `opengrep.exe` and the DLLs it needs: keep them
together in one directory.

# Build instructions for developers

## Manual development

Developers should consult the makefiles, which are documented.
The prerequisites are listed in the prelude of the toplevel makefile: OCaml
(currently 5.5.0) with `dune` and `opam`, a C toolchain with the PCRE and gmp
libraries, and common tools such as `make` and `git`.

The steps to set up and build everything are normally:

```
$ git submodule update --init --recursive
$ make setup       # meant to be run infrequently, may not be sufficient
$ make             # routine build
$ make test        # test everything
```

`make core` builds `bin/opengrep`, `bin/opengrep-cli` and `bin/opengrep-core`,
which are the same executable under three names; `opengrep-core` is the
low-level core CLI. There is nothing to install: run the build in place.

```
$ ./bin/opengrep --help
```

`make core-test` runs the tests.

The OCaml sources are in the [`src/`](src) folder.
Read the toplevel makefile to see what's available to the developer.
