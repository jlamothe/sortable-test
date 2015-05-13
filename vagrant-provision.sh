#!/bin/sh

apt-get update
apt-get install -qy cabal-install hlint

sudo -u vagrant bash --login cabal update
