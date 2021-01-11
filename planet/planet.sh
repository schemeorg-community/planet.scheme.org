#!/bin/sh
set -eu
cd "$(dirname "$0")"
[ "$(id -un)" = prod-blog ] || { echo "Run me as prod-blog"; exit 1; }
echo "Entering directory '$PWD'"
set -x
rm -rf output
planet planet.ini "$@"
rsync -vax output/ static/ ../www/
rm -rf output
