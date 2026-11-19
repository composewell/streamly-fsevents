#!/bin/sh
#
# Run the bind mount tests of the Linux test suite in an unprivileged user
# and mount namespace. A directory src and its bind mount mnt are created in
# a temporary directory, its path is passed in FSEVENTS_BIND_MOUNT.
#
# Usage: test/bind-mount.sh [test arguments]
#
# Run it from the root of the package. Requires unprivileged user namespaces
# and unshare (util-linux), see test/no-proc.sh.

set -e

# CABAL_BUILD_OPTIONS, e.g. "--flag limit-build-mem", is passed to cabal.
target=FileSystem.Event.Linux
# shellcheck disable=SC2086
cabal build --enable-tests $CABAL_BUILD_OPTIONS $target
# shellcheck disable=SC2086
bin=$(cabal list-bin --enable-tests $CABAL_BUILD_OPTIONS $target)

# shellcheck disable=SC2016
unshare -rm sh -c '
    set -e
    dir=$(mktemp -d)
    mkdir "$dir/src" "$dir/mnt"
    mount --bind "$dir/src" "$dir/mnt"
    status=0
    FSEVENTS_BIND_MOUNT=$dir "$0" --match "/bind mount/" "$@" || status=$?
    umount "$dir/mnt"
    rm -rf "$dir"
    exit $status
' "$bin" "$@"
