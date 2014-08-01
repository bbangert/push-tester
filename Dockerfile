# Mozilla SimplePush Tester

# VERSION    0.1

# Extend haskell image
FROM darinmorrison/haskell

MAINTAINER Ben Bangert <bbangert@mozilla.com>

# Copy the simplepush code into the container
ADD . /projects/simpletester

WORKDIR /projects/simpletester

RUN rm -rf .cabal-sandbox
RUN apt-get install zlib1g-dev
RUN cabal update
RUN cabal sandbox init
RUN cabal install --only-dependencies
RUN cabal configure
RUN cabal build
RUN cp dist/build/spTester/spTester /usr/bin/spTester

ENTRYPOINT ["/usr/bin/spTester"]
