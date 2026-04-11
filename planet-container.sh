#!/bin/sh
set -eu
exec 3>&1 >&2
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
rm -rf output
gosh planet.scm "$@"
rsync -r static/ output/
mv output www
tar --sort name -cf - www >&3
