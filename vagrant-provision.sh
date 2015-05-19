#!/bin/sh

apt-get update
apt-get install -qy cabal-install hlint

export HOME=/home/vagrant
sudo -u vagrant bash --login /vagrant/cabal-setup.sh
