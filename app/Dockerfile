FROM stackbrew/debian:wheezy
RUN echo "deb http://ftp.debian.org/debian sid main" >> /etc/apt/sources.list
RUN apt-get update
RUN apt-get -t sid install libc6
ADD spTester /bin/spTester
ADD libs/ /lib/
ENTRYPOINT ["/bin/spTester"]
