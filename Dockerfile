# Mozilla SimplePush Tester

# VERSION    0.1

FROM phusion/baseimage:0.9.13

MAINTAINER Ben Bangert <bbangert@mozilla.com>

# Setup Haskell
RUN apt-get update && apt-get install -yqq curl libgmp10 zlib1g-dev

WORKDIR /
RUN curl https://www.haskell.org/platform/download/2014.2.0.0/haskell-platform-2014.2.0.0-unknown-linux-x86_64.tar.gz | tar xz

# This is where ghc expects libgmp
RUN ln -s /usr/lib/x86_64-linux-gnu/libgmp.so.10.1.3 /usr/lib/x86_64-linux-gnu/libgmp.so

RUN /usr/local/haskell/ghc-7.8.3-x86_64/bin/activate-hs

# Copy the simplepush code into the container
ADD . /projects/simpletester

WORKDIR /projects/simpletester

RUN rm -rf .cabal-sandbox

RUN cabal sandbox init
RUN cabal update
RUN cabal install --only-dependencies --force-reinstalls
RUN cabal configure
RUN cabal build
RUN cp dist/build/spTester/spTester /usr/bin/spTester

ENTRYPOINT ["/usr/bin/spTester"]
