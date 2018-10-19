############################################################
# Stage 1: Build binary with stack

FROM haskell:8.4.3 as builder

WORKDIR /opt/azure-exporter

# Download and install stack build plan
COPY stack.yaml stack.yaml
RUN stack setup

# Download and install dependencies
COPY package.yaml package.yaml
RUN stack install --only-dependencies

# Copy sourcecode and build binary
COPY . /opt/azure-exporter
RUN stack build

############################################################
# Stage 2: Minimal production image

FROM debian:stretch-slim
label maintainer="Gavin Lam <me@gavin.hk>"

# Install libgmp10 for dynamically linked libgmp.so.10,
# install netbase for populating /etc/protocols,
# install ca-certificates to refresh SSL certs
RUN \
  apt-get update && \
  apt-get install -y libgmp10 netbase ca-certificates && \
  rm -rf /var/lib/apt/lists/*

# Copy compiled binary from builder
COPY --from=builder \
  /opt/azure-exporter/.stack-work/install/x86_64-linux/lts-12.10/8.4.3/bin \
  /opt/azure-exporter

WORKDIR /opt/azure-exporter

# Expose default port
EXPOSE 9492

CMD ["sh", "-c", "azure-exporter-exe", "server"]
