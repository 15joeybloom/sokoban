Sokoban
=======

Use `stack` to build and test. You'll need to make sure you have C libraries
`blas` and `lapack` installed.

There are two executables providing two different interfaces to play the game.
Look in the output of `stack build` to see where `stack` installed them.

sokoban-exe
-----------

This is the first interface I created because it was the simplest to implement.
It prints out the warehouse, using various unicode symbols to represent each
cell. The user moves using WASD or HJKL (like vim). The user must press Enter
after each move.

sokoban-better-exe
------------------

This interface uses [vty](http://hackage.haskell.org/package/vty). The user
doesn't have to press enter after each move, and the moves are drawn in-place
without having to print out the whole warehouse again.

This interface can also load a warehouse from a `.sok` file. Play a warehouse by
running

```
sokoban-better-exe warehouses/example.sok
```

Check out `warehouses/` for a ready-to-play example warehouse. If you want to
create your own warehouse, feel free to add it to `warehouses/` and create a
pull request; I will gladly merge it upstream. 


