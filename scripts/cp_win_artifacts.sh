#!/usr/bin/env bash

BASE="/cygdrive/c/cygwin64/usr/x86_64-w64-mingw32/sys-root/mingw/bin/"

mkdir -p artifacts/
rm -f artifacts/*

cp bin/* artifacts/
cp artifacts/opengrep-core.exe artifacts/opengrep.exe

# The mingw runtime, plus the native libraries opengrep-core links against.
# Keep in sync with the ldd output printed by the opengrep-core test step in
# build-test-windows-x86; everything else it needs comes from System32.
cp $BASE/libstdc++-6.dll artifacts/
cp $BASE/libgcc_s_seh-1.dll artifacts/
cp $BASE/libwinpthread-1.dll artifacts/
cp $BASE/libgmp-10.dll artifacts/
cp $BASE/libpcre2-8-0.dll artifacts/
cp $BASE/libzstd-1.dll artifacts/

# For the wheel:
cp artifacts/* cli/src/semgrep/bin
