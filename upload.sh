#!/bin/sh
set -eu
cd "$(dirname "$0")"
rsync -crv --exclude '.*' --exclude '*~' \
    ./ tuonela.scheme.org:/production/planet/
