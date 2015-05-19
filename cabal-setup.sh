#!/bin/sh

cabal update
cat /vagrant/path >> ~vagrant/.profile
