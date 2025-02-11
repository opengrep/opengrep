# Build instructions for developers

First, clone the repository and fetch the submodules:

```sh
$ git clone --recursive https://github.com/opengrep/opengrep 
```
if you've already cloned the repository, you can fetch the submodules with:
```sh
$ git submodule update --init --recursive
```


## Local development

Developers should consult the `Makefile`s, which are documented.
The steps to set up and build everything are normally:

```
$ make setup       # meant to be run infrequently, may not be sufficient
$ make             # routine build
$ make test        # test everything
```

There's no simple installation of the development version of the
`opengrep` command (Python wrapper + `opengrep-core` binary). To test
`opengrep` without installing it, use `pipenv`:

```
$ cd semgrep
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
  PIPENV_PIPFILE=~/semgrep/cli/Pipfile pipenv run opengrep "$@"
}
```

The Semgrep project has two main parts:

- The Python wrapper in the [`cli/`](cli) folder, which has its own
  makefile needed for some preprocessing and for testing.
  Read the makefile to see what targets are available.
- The OCaml core in the [`src/`](semgrep-core) folder.
  Read the toplevel makefile to see what's available to the developer.

## Reproducible and standalone build with Docker

The main [`Dockerfile`](Dockerfile) serves as a reference on how to
build Opengrep for Linux. The usual instructions for building a Docker
image apply. Once you've cloned the repository and fetched the submodules(see above), it should be:

```
$ docker build -t opengrep .
```
