#!/usr/bin/env bash

for filename in $(find ./languages/ ./libs/ocaml-tree-sitter-core/ -name dune); do
  grep -v rpath $filename > $filename.new
  mv $filename.new $filename
done
