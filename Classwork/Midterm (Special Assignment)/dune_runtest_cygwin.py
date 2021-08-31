#!/usr/bin/env python3

# Python bash script to run "eval $(opam env)" and "dune utop" since Cygwin needs
# both of them upon each closing and starting up of the program for some reason.

# Import the OS library, which gives you commands to the operating system
import os

os.system("eval $(opam env) && dune runtest")
