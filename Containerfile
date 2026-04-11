# Build Gauche from source for the feed aggregator.

from debian:bookworm-slim as build
run apt-get update && apt-get -qq install \
      ca-certificates file gcc libc-dev libgc-dev libmbedtls-dev \
      make netbase pkg-config zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*
workdir /build
copy gauche-checksum gauche-checksum
add https://github.com/shirok/Gauche/releases/download/release0_9_15/Gauche-0.9.15.tgz gauche.tgz
run sha256sum -c gauche-checksum
run mkdir gauche && tar -C gauche --strip-components 1 -xf gauche.tgz
workdir /build/gauche
run ./configure --with-tls=mbedtls
run make
run make install
copy planet.scm planet-container.sh config.scm /planet/
copy static /planet/static

from debian:bookworm-slim
run apt-get update && apt-get -qq install \
      ca-certificates libgc1 libmbedtls14 rsync \
 && rm -rf /var/lib/apt/lists/*
copy --from=build /usr/local/ /usr/local/
copy --from=build /planet/ /planet/
workdir /planet
cmd ["./planet-container.sh"]
