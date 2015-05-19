# Sortable Test

This program is my attempt at solving a test that was presented to me.
The software in this repository takes data in CSV format from standard
input, performs an analysis, and outputs the result (also in CSV) to
standard output.

## daily-summary

The `daily-summary` program generates a daily report of the
statistical data.

## Compiling

If you have cabal installed on your system, you should be able to
compile and install the software with the command `cabal install`.

## Vagrant

If you do not have Haskell and cabal installed, but you do have
Vagrant and VirtualBox installed a Vagrantfile has been included which
will build a VM with the required software and run the test suite.
You will need to issue the following commands:

    vagrant up    # start (and provision if necessary) the virtual machine
    vagrant ssh   # connect to the VM
    cd /vagrant   # switch to the /vagrant virtual directory which holds the code
    cabal install # compile and install the software to the VM

## Tests

The tests are located in the `/tests.hs` file.  They can be run with
the command

    runhaskell tests.hs
