#!/bin/sh
set -eu
cd "$(dirname "$0")"
[ "$(id -un)" = prod-planet ] || { echo "Run me as prod-planet"; exit 1; }
echo "Entering directory '$PWD'"
set -x
rm -rf output
planet planet.ini "$@"
mkdir -p output/images/cache
(cd output && gosh ../images.scm <index.html >index.html.new)
mv -f output/index.html.new output/index.html
rsync -vax output/ static/ ../www/
rm -rf output
