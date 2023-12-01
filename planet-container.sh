#!/bin/sh
set -eu
exec 3>&1 >&2
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
rm -rf output
planet planet.ini "$@"
mkdir -p output/images/cache
rsync -r static/ output/
(cd output && gosh ../images.scm <index.html >index.html.new)
mv -f output/index.html.new output/index.html
mv output www
tar --sort name -cf - www >&3
