mars_rover
=====

An Erlang OTP application that moves rovers around Mars's surface.

It takes a grid of M rows, N columns, one or more robots and a set of movements for each robot.

Build
-----
To compile the code:

    $ rebar3 compile
To start up the application and load/print out the input from the files (grid coordinates, rover position and orientation, set of movements):

    $ rebar3 shell

To execute the rover's movements in the shell:

    mars_rover:move_rover().

# Future improvements:
- validate / check case-sensitive strings
- take a list of rovers and directions (currently can only handle one rover)
- check for crashes between rovers (in the case two rovers end up on the same position)