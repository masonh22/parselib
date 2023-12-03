# Parselib

Library for parsing formatted files.

## Installation

Run `dune build` to compile

Run `opam pin add parselib .` to create and install the package.

Run `opam pin remove parselib` to remove the package.

## TODO

- Better dune-project file
- more efficient parsing with sequences; rust bindings?
- toplevel
  + this just isn't working... can't get it to build with the Parselib library
    for some reason. need to do this to get it to work:
`
#directory "<path-to-project>/_build/default/lib/.parselib.objs/byte";;
#load "<path-to-project>/_build/default/lib/parselib.cma";;
`
