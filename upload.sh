#!/bin/sh
set -eu
cd "$(dirname "$0")"
rsync -crv --exclude '*~' planet/ alpha.servers.scheme.org:/production/planet/planet/
