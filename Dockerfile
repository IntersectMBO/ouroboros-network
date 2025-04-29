FROM haskell:9.10

# Copy over kes-agent source files
ADD ./kes-agent /opt/kes-agent/kes-agent/
ADD ./cabal.project /opt/kes-agent/
ADD ./docker/cabal.project.local /opt/kes-agent/

# Copy kes-agent bootstrapping data
ADD ./kes-agent/fixtures/mainnet-shelley-genesis.json /etc/kes-agent/mainnet-shelley-genesis.json
ADD ./docker/agent.toml /etc/kes-agent/agent.toml

# Install system dependencies
RUN apt-get update
RUN apt-get install -y libsodium-dev autotools-dev autoconf build-essential libtool pkgconf

# secp256k1
WORKDIR /opt
RUN git clone https://github.com/bitcoin-core/secp256k1.git
WORKDIR /opt/secp256k1
RUN ./autogen.sh
RUN ./configure
RUN make
RUN make install

# blst
WORKDIR /opt
RUN git clone https://github.com/supranational/blst
WORKDIR /opt/blst
RUN git checkout v0.3.14
RUN ./build.sh
RUN install ./libblst.a /usr/local/lib/
RUN install ./bindings/blst_aux.h /usr/local/include/
RUN install ./bindings/blst.h /usr/local/include/
ADD ./docker/libblst.pc /usr/local/lib/pkgconfig/libblst.pc

RUN ldconfig

# Build kes-agent
WORKDIR /opt/kes-agent
RUN cabal update
RUN cabal build exe:kes-agent
RUN cabal install exe:kes-agent --installdir=/usr/local/bin/ --install-method=copy --overwrite-policy=always

CMD /usr/local/bin/kes-agent run
