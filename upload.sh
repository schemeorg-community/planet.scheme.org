#!/bin/sh
set -eu
cd "$(dirname "$0")"
rsync -vrx --exclude '*~' planet/ alpha.servers.scheme.org:/production/planet/planet/
