Conway's Game of Life implemented in Haskell using repa and SDL
==========

About 
----------

This is a Haskell implementation of Conway's Game of Life that uses repa
for efficient arrays and parallelization, and SDL for graphics and 
keyboard/mouse input

Usage
---------

Build this package with

~~~~
cabal configure
cabal build
~~~~

And run it with

~~~~
cabal run <cell size> <path to life_1.06 pattern file> +RTS -Nx
~~~~

Where x is the number of cores of your computer.

First two arguments are optional.

Keyboard
---------

SPACE
: Play/Pause

r
: Random grid

c
: Clear grid

RETURN
: Exit

Click
: Toggle cell
