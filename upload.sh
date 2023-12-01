#!/bin/sh
set -eu
cd "$(dirname "$0")"
rsync -crv --exclude '*~' planet/ tuonela.scheme.org:/production/planet/planet/
