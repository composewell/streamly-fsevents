#!/bin/sh
#
# Run the Linux test suite with /proc hidden, in an unprivileged user and
# mount namespace:
#
# * enoent: a tmpfs is mounted over /proc, /proc/self/fd does not exist.
# * eacces: a tmpfs of mode 000 is mounted over /proc and the capabilities
#   are dropped, access to /proc/self/fd is denied.
#
# Usage: test/no-proc.sh [enoent|eacces] [test arguments]
#
# Without a mode both the modes are run. Requires unprivileged user
# namespaces, unshare and setpriv (util-linux). On Ubuntu 24.04 the AppArmor
# restriction must be lifted first:
#
#   sudo sysctl kernel.apparmor_restrict_unprivileged_userns=0

set -e

case "$1" in
    enoent|eacces) modes=$1; shift ;;
    *) modes="enoent eacces" ;;
esac

cabal build FileSystem.Event.Linux
bin=$(cabal list-bin FileSystem.Event.Linux)

# The inotify limits are not per namespace, the tests read the limit from
# the environment when /proc is not available.
FSEVENTS_MAX_QUEUED_EVENTS=$(cat /proc/sys/fs/inotify/max_queued_events)
export FSEVENTS_MAX_QUEUED_EVENTS

status=0
for mode in $modes; do
    echo "=== /proc not available: $mode"
    case $mode in
        enoent)
            unshare -rm sh -c 'mount -t tmpfs none /proc && exec "$0" "$@"' \
                "$bin" "$@" || status=1
            ;;
        eacces)
            unshare -rm sh -c \
                'mount -t tmpfs -o mode=000 none /proc &&
                 exec setpriv --inh-caps=-all --bounding-set=-all "$0" "$@"' \
                "$bin" "$@" || status=1
            ;;
    esac
done
exit $status
