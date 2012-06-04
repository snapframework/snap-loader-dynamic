Snap Framework
==============

Snap is a web framework for Haskell, based on iteratee I/O (as [popularized by
Oleg Kiselyov](http://okmij.org/ftp/Streams.html#iteratee)).  For more
information about Snap, read the `README.SNAP.md` or visit the Snap project
website at http://www.snapframework.com/.

## Library contents

This is utility project for the Snap Framework, which contains a
library allowing Snap applications to recompile actions on the fly in
development mode, with no performance loss in production mode.


Building snap
=============

This library is built using
[Cabal](http://www.haskell.org/cabal/) and
[Hackage](http://hackage.haskell.org/packages/hackage.html). Just run

    cabal install

from the `snap-dynamic` toplevel directory.


## Building the Haddock Documentation

The haddock documentation can be built using 'cabal haddock'.

The docs get put in `dist/doc/html/`.

