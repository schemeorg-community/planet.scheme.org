# Buster is the last version of Debian that packages planet-venus.
# Perhaps because planet-venus uses Python2 which has since been
# dropped from Debian.
#
# The version of Gauche packaged in Buster is not recent enough to
# have SRFI 115 (Scheme Regular Expressions). Hence we build a newer
# Gauche from source.
#
# Building Gauche from source is easier than building Python2 and
# planet-venus from source.

from debian:buster-slim as build
run apt-get update && apt-get -qq install \
      ca-certificates file gcc libc-dev libgc-dev libmbedtls-dev \
      make netbase pkg-config zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*
workdir /build
copy checksum checksum
add https://files.scheme.org/Gauche-0.9.10.tgz gauche.tgz
run sha256sum -c checksum
run mkdir gauche && tar -C gauche --strip-components 1 -xf gauche.tgz
workdir /build/gauche
run ./configure --with-tls=mbedtls
run make
run make install
copy images.scm planet-container.sh planet.ini /planet/
copy static /planet/static
copy templates /planet/templates

from debian:buster-slim
run apt-get update && apt-get -qq install \
      ca-certificates libgc1c2 libmbedtls12 planet-venus rsync \
 && rm -rf /var/lib/apt/lists/*
copy --from=build /usr/local/ /usr/local/
copy --from=build /planet/ /planet/
workdir /planet
cmd ["./planet-container.sh"]
