# ## Overview
#
# This is a Nix file for Opengrep developers.
#
# Nix is a dependency manager. Nix allows to use just one command
# to get a fully functional correct and near identical development
# environment across any OS, instead of the 10-20 commands it normally takes.
# It's totally opt-in and will not impact anyones work flow.
# See https://shopify.engineering/what-is-nix for more information.
#
# ## Quick install of Nix
#
# Use https://github.com/DeterminateSystems/nix-installer to get Nix easily
# then run `nix develop` to get a shell with all the dependencies
#
# Quick start:
# To use your shell when developing:
# nix develop -c $SHELL
#
# To disallow all deps outside of Nix:
# nix develop -i
#
# What is Nix?
#
# Nix is a package dependency manager for reproducible and correct builds. Nix
# is structured around the concept that each dependency is a function of its
# build dependencies. For example a hello world program takes in GCC and make as
# function arguments, and spits out a hello world binary. What makes this
# reproducible is that nix treats files as pointers, and produces a closure over
# all files in a build system. For example if the hello world depends on a C
# library, then your nix build of hello world won't complete unless you
# explicitly declare said library in your nix configuration. Since all build
# tools made with nix are built this way, nix knows exactly what any dependency
# needs to build, and to run, meaning *all builds are easily reproducible, and
# are not affected by what you may or may not have installed on your system*.
#
# ### What is the difference with a Dockerfile?
#
# A Dockerfile is a great way to build an entire OS, and run a program in that
# and can be used for development, but has many downsides.
#
# Docker still relies on Linux, and that Linux distro still relies on some
# package manager. This means that even if you use Docker, your environment is
# still not reproducible as packages are not built deterministically like Nix.
# So you can create a Dockerfile building Opengrep, but you will need a different
# build script + Dockerfile for every system, and you must determine what the
# different distributions all ship and have package wise. Meanwhile this Nix
# file will work (almost) exactly the same across any system.
#
# Docker on many systems like anything Apple Silicon will run an emulator, which
# is very high perf overhead for day to day development. Then your tooling also
# won't take advantage of system specific speedups.
#
# Docker also is a completely different system, so if you are testing/developing
# for Macs, Docker can't help there. Additionally since it is a different system
# you don't have access to any of your tools, such as your IDE, favorite shell
# etc. unless you want to set up a mount + ssh everytime, which is a lot of
# friction.
#
# Think: why is Rust's Cargo, Javascript's NPM, Python's Poetry so popular?
# Because they explicitly declare the dependencies, instead of hoping you have
# said dependencies installed already, or shipping a bash script/Makefile that
# installs them for you.
#
# ## Why should I care?
#
# Our OCaml code is (mostly) correct because that's a focus of the language
# itself. But it relies on a lot of C code and external dependencies, like
# `libev`, `libcurl`, `tree-sitter` etc. This means that if someone wants to
# build and contribute to Opengrep, they must install these dependencies, and
# hope that their versions is compatible. For example if you install the OCaml
# packages needed for Opengrep, and then install libev, things won't work, since
# the lwt package needs libev when its initially installed. If you're on mac,
# you also have to tell opam where libev is before installing lwt. So our OCaml
# is correct, but only if you can build it, and only if you build it with the
# right dependencies.
#
# By using Nix, we can declare all these dependencies explicitly, and then
# anyone across any *nix system can easily build Opengrep with only one command!
# What's even better, is for regular contributors, these dependencies will auto
# update and rebuild whenever a new dependency is added. So if someone adds a
# dependency for lwt, libev will automatically be pulled in with 0 thought from
# the contributor, and no need to run `make dev-setup` or anything similar.
# Since all of the dependencies are reproducible, they're almost identical no
# matter what system you're on, so if someone runs into a bug in developing, you
# can have more confidence in eliminating a system difference as the source of
# the bug.
#
# Another bonus is that it's easy to know what version of a dependency you're
# on, e.g. what version of tree-sitter, or what version of OCaml. You can see
# how this also would be good for security :).
#
# There's a lot more places this can simplify things for us, like CI and
# releases. See future work for more details.
#
# Finally, nix configurations are written in a functional programming language.
# Don't you want to declare dependencies in a functional language?
#
# ## Who uses Nix?
#
# Not convinced? Here are some other notable OCaml projects using nix:
# - [dune](https://github.com/ocaml/dune)
# - [merlin](https://github.com/ocaml/merlin)
# - [ocaml-lsp](https://github.com/ocaml/ocaml-lsp)
# - [ocaml-re](https://github.com/ocaml/ocaml-re)
#
# There's also a ton of projects outside the OCaml world using nix too. Nix is
# battle tested, and has a huge community, with support for almost anything you
# can think of. There's even someone packaging Opengrep for nix.
#
# ## Try it out
#
# If you want to see what it's like to use nix, get a clean Linux or macOS box.
# Then [install nix](https://github.com/DeterminateSystems/nix-installer) with
# flake support. Finally run `nix develop`. Now you can run `make core` or any
# make targets in the CLI. That's one command to always be able to build Opengrep
# vs the 10-20 needed right now, if you're lucky, and then 2-3 every once in
# awhile to keep up to date.
#
# If you want to reproduce and help with a bug in someones branch, you can just
# run `nix develop` and `make test` to instantly have the same exact environment
# they ran into the bug in. If you're really unsure about if it's a system bug,
# you can run `nix develop -i` to exclude any non nix dependencies completely.
#
# If you don't use bash run `nix develop -c $SHELL` to get an environment with
# your shell.
#
# If you want to just build opengrep and run it you can do `nix run` for opengrep
# or `nix run ".#<target>"` where target is opengrep or pyopengrep.
#
# ## What's the catch?
#
# There's not a real big catch to using nix, except development time to set it
# up, which the PR that introduced this did. A few small catches are that nix can be a little
# slow on a first build, but no more than say `make dev-setup`, and that it uses
# a decent amount of storage space, but again, not much more than any other
# package manager.
#
# ## Contributing/Maintennance
#
# The maintenance for nix is super low. It automatically pulls in any dependency
# declared in our semgrep.opam file, which then pulls in any non ocaml
# dependencies those dependencies rely on. If someone adds a new build tool or
# python package, they will have to add the dependency to `flake.nix`. This
# means you have to read some comments then add the name of the package to an
# array somewhere, super easy. If you're interested in doing anything more
# complex, see the future reading section.
#
# ## CI testing
#
# If people like this tool, at some point it'd be great to add a CI workflow
# that ensures all PRs work even with nix. But let's see how this goes first.
#
# ## Improving our CI
#
# Where nix would smooth things over for us even more is simplifying our CI.
# Right now we have to guess and hope that our CI has the same dependencies as
# what's on our system, and it can be a fight to add any new dependencies, like
# libcurl. Not to mention our workflows differ by system, and it's a pain to
# make sure they all have the right package names. If we use nix in CI, all we
# need is to install nix on the target OS/architecture, then run nix build and
# it can run all build processes, and all e2e tests. Super simple workflow. It
# can even build the wheels, and static versions of Opengrep we'd want!
#
# # More reading
#
# Interested in reading more about nix? Here are some hand sources
#
# - [What is Nix](https://shopify.engineering/what-is-nix) - short intro to nix
#   by Shopify Engineering
#
# - [Nix Pills](https://nixos.org/guides/nix-pills/) -long form intro to nix
#
# - [Nix Flakes](https://zero-to-nix.com/concepts/flakes) - intro to the format
#   we use to configure nix
#
# - [opam-nix](https://www.tweag.io/blog/2023-02-16-opam-nix/) - how we nixify
#   opam deps
#
# - [Nix PhD Thesis](https://edolstra.github.io/pubs/phd-thesis.pdf) - Nix
#   creator's PhD thesis on Nix. ~275 pages but really approachable
{
  description = "Opengrep is an ultra-fast static analysis tool for searching code patterns with the power of semantic grep.";
  inputs = {
    self.submodules = true;
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
  };
  outputs = {
    nixpkgs,
    flake-utils,
    opam-nix,
    opam-repository,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      on = opam-nix.lib.${system};
      pythonPackages = pkgs.python311Packages;
      opamRepos = [opam-repository];
      lib = pkgs.lib;
      isDarwin = lib.strings.hasSuffix "darwin" system;
    in let
      # opengrep inputs
      devOpamPackagesQuery = {
        # You can add "development" ocaml packages here. They will get added to the devShell automatically.
        ocaml-lsp-server = "*";
        utop = "*";
        ocamlformat = "*";
        earlybird = "*";
        merlin = "*";
      };
      opamQuery =
        devOpamPackagesQuery
        // {
          # You can force versions of certain packages here
          ocaml-base-compiler = "5.3.0";
          mirage-runtime = "4.9.0";

          # coupling: if you add one thing here, need to update also the buildInputs overlay below
          git-unix = "*";
          junit_alcotest = "*";
          notty = "*";
          tsort = "*";
          tyxml = "*";
        };

      scope =
        on.buildOpamProject' {
          pkgs = pkgs; # to force newest version of nixpkgs instead of using opam-nix's
          repos = opamRepos; # to force newest version of opam
        }
        ./.
        opamQuery;
      scopeOverlay = final: prev: {
        # You can add overrides here
        conf-pkg-config = prev.conf-pkg-config.overrideAttrs (prev: {
          # We need to add the pkg-config path to the PATH so that dune can find it
          nativeBuildInputs = prev.nativeBuildInputs ++ [pkgs.pkg-config];
        });
        semgrep = prev.semgrep.overrideAttrs (prev: {
          # Prevent the ocaml dependencies from leaking into dependent environments
          doNixSupport = false;
          buildInputs =
            prev.buildInputs
            ++ [
              final.git-unix
              final.junit_alcotest
              final.notty
              final.tsort
              final.tyxml
            ];
        });
      };
      scope' = scope.overrideScope scopeOverlay;

      # Packages for development
      devOpamPackages =
        builtins.attrValues
        (pkgs.lib.getAttrs (builtins.attrNames devOpamPackagesQuery) scope');

      # Package with all opam deps but nothing else
      baseOpamPackage = scope'.semgrep;

      # Special environment variables for linking
      opengrepEnvDarwin = {
        # all the dune files of semgrep treesitter <LANG> are missing the
        # :standard field. Basically all compilers autodetct if something is c
        # or c++ based on file extension, and add the c stdlib based on that.
        # Nix doesn't because reasons:
        # https://github.com/NixOS/nixpkgs/issues/150655 Dune also passes
        # -xc++ if it detects a c++ file (again sane), but it's included in
        # the :standard var, which we don't add because ??? TODO add and
        # commit them instead of doing this
        NIX_CFLAGS_COMPILE = "-I${pkgs.libcxx.dev}/include/c++/v1";
      };
      opengrepEnv =
        {
          SEMGREP_NIX_BUILD = "1";
        }
        // lib.optionalAttrs isDarwin opengrepEnvDarwin;

      #
      # opengrep
      #

      opengrep = baseOpamPackage.overrideAttrs (prev: {
        pname = "opengrep";
        env = opengrepEnv;

        buildInputs =
          prev.buildInputs
          ++ (with pkgs; [
            pcre2
            tree-sitter
          ]);
        buildPhase = ''
          make core
        '';

        nativeCheckInputs = with pkgs; [cacert git];
        # git init is needed so tests work successfully since many rely on git root existing
        checkPhase = ''
          git init
          make test
        '';

        # Copy opengrep binaries
        installPhase = ''
          mkdir -p $out/bin
          cp _build/install/default/bin/* $out/bin
        '';
      });

      # pyopengrep inputs
      # coupling: anything added for testing should be added here
      devPipInputs = with pythonPackages; [
        pkgs.git
        flaky
        pytest-snapshot
        pytest-mock
        pytest-freezegun
        types-freezegun
      ];

      #
      # pyopengrep
      #

      pyopengrep = with pythonPackages;
        buildPythonApplication {
          pname = "pyopengrep";
          inherit (opengrep) version;
          src = ./cli;

          pyproject = true;
          build-system = [setuptools];

          pythonRelaxDeps = [
            "boltons"
            "defusedxml"
            "exceptiongroup"
            "glom"
            "rich"
            "tomli"
            "wcmatch"
          ];

          # coupling: anything added to the pysemgrep setup.py should be added here
          propagatedBuildInputs = [
            attrs
            boltons
            click
            click-option-group
            colorama
            defusedxml
            exceptiongroup
            glom
            jsonschema
            packaging
            peewee
            requests
            rich
            ruamel-yaml
            tomli
            tqdm
            typing-extensions
            urllib3
            wcmatch
            protobuf
            jaraco-text
          ];

          preFixup = ''
            makeWrapperArgs+=(--prefix PATH : ${opengrep}/bin)
          '';
        };
    in {
      # For a lot of nix commands, nix uses the cwd's flake. So
      #   nix develop
      # will run the current flake.
      #
      # The target of the command is structured
      # `<FLAKE_LOCATION>(?OPTIONAL_PARAMS)#<TARGET>` so you can run
      #   nix run nixpkgs#gcc
      # to run gcc from the default nix repository nixpkgs.
      #
      # But the location is similar to a url! So you can run
      #   nix run github:DeterminateSystems/flake-checker
      # to run DeterminateSystem's
      # cool flake checker, without ever needing to install it or anything! Or
      # other nix users who want to try opengrep can run
      #   nix run github:opengrep/opengrep
      #
      # See: https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html#types
      # for more on flake refs
      #
      # See: https://nixos.org/manual/nix/stable/command-ref/experimental-commands
      # for other useful commands

      #   nix build ".#<PKG_NAME>"
      # builds the below package, leaving it empty builds the default. The
      # output will be linked into the cwd in a folder called "result". Also
      # exports packages for other nix packages to use
      packages = {
        opengrep = opengrep;
        pyopengrep = pyopengrep;
        default = pyopengrep;
      };

      #   nix run ".#<PKG_NAME>"
      # builds and runs the package specified, without linking the output
      # result into the cwd. You can try other nixpkgs similarly by running
      # `nix run nixpkgs#<PKG_NAME>` like `nix run nixpkgs#hello_world`.
      apps = {
        opengrep = {
          type = "app";
          program = "${opengrep}/bin/opengrep";
        };
        pyopengrep = {
          type = "app";
          program = "${opengrep}/bin/pyopengrep";
        };
        default = {
          type = "app";
          program = "${opengrep}/bin/pyopengrep";
        };
      };

      #   nix flake check
      # makes sure the flake is a valid structure, all the derivations are
      # valid, and runs anyting put in checks
      checks = {
        check-opengrep = opengrep.overrideAttrs (prev: {
          # We don't want to force people to run the test suite everytime they
          # build opengrep, but we do want to run it here
          doCheck = true;
        });
      };

      #   nix fmt
      # formats this file. In the future we can add ocaml, python, and other
      # formatters here to run also
      formatter = pkgs.alejandra;

      #   nix develop -c $SHELL
      # runs this shell which has all dependencies needed to make opengrep
      devShells.default = pkgs.mkShell {
        env =
          {
            # add env vars here
          }
          // opengrepEnv;
        inputsFrom = [opengrep pyopengrep];
        buildInputs =
          devOpamPackages
          ++ devPipInputs
          ++ (with pkgs; [
            pre-commit
            pipenv
            yq-go # for GHA workflows
          ]);
      };
    });
}
