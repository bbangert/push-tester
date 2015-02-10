#!/bin/bash
docker build -t bbangert/sptesterbuild .
docker run -v /var/run/docker.sock:/var/run/docker.sock -v $(which docker):$(which docker) -ti --name sptesterbuild  bbangert/sptesterbuild