#!/bin/sh
set -eu
cd "$(dirname "$0")"
[ "$(id -un)" = prod-blog ] || { echo "Run me as prod-blog"; exit 1; }
echo "Entering directory '$PWD'"
set -x
rm -rf www.new/
planet planet.ini "$@"
rsync -vax static/ www.new/
rsync -vax www.new/ ../www/
rm -rf www.new/
