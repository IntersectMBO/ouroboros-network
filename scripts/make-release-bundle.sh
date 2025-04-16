#!/bin/bash

REFDIR=$(dirname "$0")

mkdir -p "$REFDIR/../dist-build"
mkdir -p "$REFDIR/../dist"

ARCH="$(uname -m)"
VERSION="$(git log -n1 --format=%h)"
SRCDIR="$(realpath "$REFDIR"/../systemd)"
BUILD_NAME="kes-agent-$VERSION-$ARCH"
BUILD_ROOT="$(realpath "$REFDIR/../dist-build")"
BUILDDIR="$BUILD_ROOT/$BUILD_NAME"
DSTDIR="$(realpath "$REFDIR"/../dist)"
TARBALL="$DSTDIR/$BUILD_NAME.tar.gz"

mkdir -p "$BUILDDIR" || exit 1
mkdir -p "$BUILDDIR/bin" || exit 1
mkdir -p "$DSTDIR" || exit 1

cp -R "$SRCDIR/etc" "$BUILDDIR/" || exit 1
cp -R "$SRCDIR/install.sh" "$BUILDDIR/" || exit 1

cabal install exe:kes-agent --install-method=copy --installdir="$BUILDDIR/bin" || exit 1

cd "$BUILD_ROOT"
tar -cf "$TARBALL" "$BUILD_NAME"
