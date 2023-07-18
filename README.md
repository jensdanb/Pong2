# Pong2
Simple Pong game written in Haskell with Gloss 2D graphics library

To run, for now you need Haskell and Cabal installed, make a Cabal project, copy this code to it and run. 
I might make make an easier way later. 

The Launcher module does almost nothing, you can ignore it. The Main module calls the launcher and then the GUI module and that's about it.

So most of the logic is in src/GUI and src/Physics. They are separated so that the GUI module is for IO and the Physics module is pure. 
