#!/bin/sh
set -eu
cd "$(dirname "$0")"
rsync -vrx planet/ alpha.servers.scheme.org:/production/blog/planet/
