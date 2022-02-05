mars_rover
=====

An Erlang OTP application that moves rovers around Mars's surface.

It takes a grid of M rows and N columns, one or more rovers and a set of movements for each rover.

Build
-----
To compile the code:

    $ rebar3 compile
To start up the application and load/print out the input from the files (grid coordinates, rover position and orientation, set of movements):

    $ rebar3 shell

To execute the rover's movements in the shell:

    mars_rover:move().

# Future improvements:
- validate / check case-sensitive strings (directions and orientations)
- take a list of rovers and movements (currently can only handle one rover)
- check for crashes between rovers (in the case two rovers end up on the same position)