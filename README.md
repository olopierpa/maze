# maze: a small graphic toy
=============================
This is a little program implementing the cellular automaton described at http://www.conwaylife.com/w/index.php?title=Maze
and a group of similar automata, including the Game of Life.

It has been tested on Windows and on Linux. It should work on any platform supported by the Ocaml Graphics module.
Compile with:

ocamlopt -o maze.exe graphics.cmxa unix.cmxa maze.ml
