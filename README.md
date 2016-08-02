Snap Framework
==============

[![Build Status](https://travis-ci.org/snapframework/snap-loader-dynamic.svg?branch=master)](https://travis-ci.org/snapframework/snap-loader-dynamic)

Snap is a simple and fast web development framework and server written in
Haskell.  For more information about Snap, read the `README.SNAP.md` or visit
the Snap project website at http://www.snapframework.com/.

## Library contents

This is utility project for the Snap Framework, which contains a
library allowing Snap applications to recompile actions on the fly in
development mode.

Building snap-loader-dynamic
============================

This library is built using
[Cabal](http://www.haskell.org/cabal/) and
[Hackage](http://hackage.haskell.org/packages/hackage.html). Just run

    cabal install

from the `snap-loader-dynamic` toplevel directory.


## Building the Haddock Documentation

The haddock documentation can be built using 'cabal haddock'.

The docs get put in `dist/doc/html/`.
