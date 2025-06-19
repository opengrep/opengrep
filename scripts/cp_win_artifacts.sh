#!/usr/bin/env bash

BASE="/cygdrive/c/cygwin64/usr/x86_64-w64-mingw32/sys-root/mingw/bin/"

mkdir -p artifacts/
rm -f artifacts/*

cp bin/* artifacts/
cp artifacts/opengrep-core.exe artifacts/opengrep.exe

cp $BASE/libstdc++-6.dll artifacts/
cp $BASE/libgcc_s_seh-1.dll artifacts/
cp $BASE/libwinpthread-1.dll artifacts/
cp $BASE/libpcre-1.dll artifacts/
cp $BASE/libgmp-10.dll artifacts/
cp $BASE/libcurl-4.dll artifacts/
cp $BASE/libpcre2-8-0.dll artifacts/
cp $BASE/libeay32.dll artifacts/
cp $BASE/libidn2-0.dll artifacts/
cp $BASE/libnghttp2-14.dll artifacts/
cp $BASE/libssh2-1.dll artifacts/
cp $BASE/ssleay32.dll artifacts/
cp $BASE/libzstd-1.dll artifacts/
cp $BASE/zlib1.dll artifacts/
cp $BASE/iconv.dll artifacts/
cp $BASE/libintl-8.dll artifacts/
# Temporary hack, requires AWS CLI to be installed just for these .dll files:
cp /cygdrive/c/Program\ Files/Amazon/AWSCLIV2/libcrypto-3.dll artifacts/
cp /cygdrive/c/Program\ Files/Amazon/AWSCLIV2/libssl-3.dll artifacts/

# For the wheel:
cp artifacts/* cli/src/semgrep/bin
