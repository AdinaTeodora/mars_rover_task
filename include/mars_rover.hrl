%%%-------------------------------------------------------------------
%%% @author teodoraardeleanu
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% Definitions of grid and rover records/movements shared across modules.
%%% @end
%%% Created : 01. Feb 2022 09:13
%%%-------------------------------------------------------------------
-author("teodoraardeleanu").

-record(grid, {
  rows :: integer(),
  cols :: integer()
}).

-record(rover, {
  x_pos       :: integer(),
  y_pos       :: integer(),
  orientation :: string()
}).

%% Rover Movements
-define(LEFT,    "L").
-define(RIGHT,   "R").
-define(FORWARD, "F").
