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

# Build instructions for developers

## Manual development

Developers should consult the makefiles, which are documented.
The steps to set up and build everything are normally:

```
$ git submodule update --init --recursive
$ make setup       # meant to be run infrequently, may not be sufficient
$ make             # routine build
$ make test        # test everything
```

There's no simple installation of the development version of the
`opengrep` command (Python wrapper + `opengrep-core` binary). To test
`opengrep` without installing it, use `pipenv`:

```
$ cd cli
$ pipenv shell
$ opengrep --help
```

Or more conveniently, you can create a shell function that will call
`pipenv` from the correct location. For example, if you cloned the
`opengrep` repo in your home folder (`~`), you can place the following
code in your `~/.bashrc` file and then use `opengrep-dev` as your
`opengrep` command:

```
opengrep-dev() {
  PIPENV_PIPFILE=~/opengrep/cli/Pipfile pipenv run opengrep "$@"
}
```

The Opengrep project has two main parts:

- The Python wrapper in the [`cli/`](cli) folder, which has its own
  makefile needed for some preprocessing and for testing.
  Read the makefile to see what targets are available.
- The OCaml core in the [`src/`](src) folder.
  Read the toplevel makefile to see what's available to the developer.
