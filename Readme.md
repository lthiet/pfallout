# Introduction

This project was created as required by the concluding course "PROJET PFA"
in which our knowledge and skill with the Ocaml language 
and functional programming were tested following a 6 months course
on advanced functional programming.

This project is a turn based game, **heavily** inspired by *Civilization VI*. It was 
created by me and my teammate Kenyu KOBAYASHI.

# Compiling the game

In order to compile the game, you will need `opam` and the `tsdl`. Please enter these commands in your terminal :

```shell
> opam init
> eval $(opam config env)
> opam switch 4.07.0
> eval $(opam config env)
> opam install tsdl tsdl-mixer tsdl-image tsdl-ttf
```

(Taken from here : https://www.lri.fr/~conchon/PFA/tactical.pdf)

If any problem occurs during the last command line, you should see that it is required to install a few `SDL` libraries.
`opam` will recommend that you run a command that looks like this : `opam depext ...`. Please run it and it will
tell you which libraries you need to install in order to install `tsdl`.

As soon as possible I will include the exact libraries needed in order to compile the game

Once everything is installed, you can compile the game.

```shell
> make
```

You can then run the game.

```shell
> ./trpg.native
```

# The game

The goal of the game is simple. 

1. Get the nuclear bombs before the enemy does
2. Blow up their cities
3. Kill any survivors

If any of these step has not been completed, you will probably lose.

## Controls

* `Z S Q D ` to move the camera around. `W S A D ` if you have a QWERTY keyboard.
* `W X` to zoom in/out. `Z X` for QWERTYs.
* `Enter` to select a unit.
* `Y U I O P` to select an action. In the same order
    1. Drop a nuclear bomb (you must pick one first)
    2. Use a medikit
    3. Pick an item
    4. Attack a nearby unit
    5. Move a unit
* `,` to open up the in-game menu. `M` for QWERTYs.
* `R` to end your turn.
* Arrows allow you to move the cursor around.

In order to execute an action, it may a little bit *heavy*. Here are the things you need to do:

1. Select a unit
2. Select an action
3. Select a destination
4. Press enter to confirm

# Further details

If you are curious about the entrails of the game, feel free to compile `rapport.tex`. You'll have
to be fluent in French however. Sorry...

# Credits

All the art asset were made by me.

A good chunk of the pseudo code for hex-based grid are from this website : https://www.redblobgames.com/grids/hexagons/

Music is : https://soundcloud.com/leagueoflegends/omega-squad-teemo

# What's next?

Unfortunately, I had to rush this game in the last few days. As a result, a lot of what I call *Fast food* code still remains.
Once my burnout is over, I will clean the code base and open the project for public contributions in the near future.
