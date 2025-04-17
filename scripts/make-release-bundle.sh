#!/bin/bash

REFDIR=$(dirname "$0")

mkdir -p "$REFDIR/../dist-build"
mkdir -p "$REFDIR/../dist"

SRCDIR="$(realpath "$REFDIR/../systemd")"
BUILD_ROOT="$(realpath "$REFDIR/../dist-build")"

cd "$REFDIR/.."

cabal build exe:kes-agent || exit 3

KES_AGENT_BINARY=$(cabal list-bin exe:kes-agent 2>/dev/null | tail -n1)
if [ "$KES_AGENT_BINARY" == "" ]; then
    echo "Could not find kes-agent binary after build" >&2
    exit 2
fi

echo "kes-agent found at:"
echo "$KES_AGENT_BINARY"

ARCH="$(uname -m)"
VERSION=$("$KES_AGENT_BINARY" --version | sed -e 's/ /-/')
if [ "$VERSION" == "" ]; then
    echo "Could not determine version" >&2
    exit 2
fi
BUILD_NAME="$VERSION-$ARCH"
BUILDDIR="$BUILD_ROOT/$BUILD_NAME"
DSTDIR="$(realpath "$REFDIR"/../dist)"
TARBALL="$DSTDIR/$BUILD_NAME.tar.gz"

echo "Building release bundle: $BUILD_NAME"

mkdir -p "$BUILDDIR" || exit 1
mkdir -p "$BUILDDIR/bin" || exit 1
mkdir -p "$DSTDIR" || exit 1

echo "Copying files..."

cp "$KES_AGENT_BINARY" "$BUILDDIR/bin/" || exit 1
cp -R "$SRCDIR/etc" "$BUILDDIR/" || exit 1
cp -R "$SRCDIR/install.sh" "$BUILDDIR/" || exit 1

echo "Creating tarball $(basename "$TARBALL")..."

cd "$BUILD_ROOT"
tar -cf "$TARBALL" "$BUILD_NAME"

echo "All done."
