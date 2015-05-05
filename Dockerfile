# Mozilla SimplePush Tester

# VERSION    0.1

FROM phusion/baseimage:0.9.13

MAINTAINER Ben Bangert <bbangert@mozilla.com>

# Setup Haskell
RUN apt-get update && apt-get install -yqq curl libgmp10 zlib1g-dev git wget openssl libssl-dev

WORKDIR /
RUN wget https://www.haskell.org/platform/download/2014.2.0.0/haskell-platform-2014.2.0.0-unknown-linux-x86_64.tar.gz
RUN tar fxz haskell-platform-2014.2.0.0-unknown-linux-x86_64.tar.gz

# This is where ghc expects libgmp
RUN ln -s /usr/lib/x86_64-linux-gnu/libgmp.so.10.1.3 /usr/lib/x86_64-linux-gnu/libgmp.so

RUN /usr/local/haskell/ghc-7.8.3-x86_64/bin/activate-hs

# Copy the simplepush code into the container
ADD . /projects/simpletester

WORKDIR /projects
RUN git clone https://github.com/bbangert/wreq.git

WORKDIR /projects/wreq
RUN git checkout --detach ebe506cc25c5cc8bb452be6122bab01d9c750c95

WORKDIR /projects/simpletester

RUN rm -rf .cabal-sandbox

RUN cabal sandbox init
RUN cabal update
RUN cabal sandbox add-source ../wreq
RUN cabal install --only-dependencies --force-reinstalls
RUN cabal configure
RUN cabal build
RUN cp dist/build/spTester/spTester app/
RUN mkdir -p app/libs
RUN cp /usr/lib/x86_64-linux-gnu/libgmp.so.10* app/libs/
RUN cp /usr/lib/x86_64-linux-gnu/librt.so* app/libs/
RUN cp /lib/x86_64-linux-gnu/libssl.so* app/libs/
RUN cp /lib/x86_64-linux-gnu/libcrypto.so* app/libs/
RUN cp /lib/x86_64-linux-gnu/libz.so* app/libs/

CMD docker build -t bbangert/simpletest /projects/simpletester/app
