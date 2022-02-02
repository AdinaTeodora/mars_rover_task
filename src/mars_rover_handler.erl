%%%-------------------------------------------------------------------
%%% @author teodoraardeleanu
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% Contains grid creation, rover creation and rover moving actions.
%%% @end
%%% Created : 31. Jan 2022 18:41
%%%-------------------------------------------------------------------
-module(mars_rover_handler).
-author("teodoraardeleanu").

-include("mars_rover.hrl").

%% Rover movements
-export([get_grid/1, get_rover/2, rotate/2, move_forward/2]).

%% Input validation
-export([is_grid_valid/1, is_rover_valid/1, are_directions_valid/1]).

%%%===================================================================
%% External functions
%%%===================================================================

%%%----------------------------------------------------
%% Get grid values starting from row = 0 and col = 0.
%%%----------------------------------------------------
get_grid({M, N}) when is_integer(M) andalso is_integer(N) ->
  #grid{rows = M - 1, cols = N - 1};
get_grid(_) ->
  {error, invalid_input}.

%%%----------------------------------------------------
%% Get rover position and orientation.
%%%----------------------------------------------------
get_rover(#rover{x_pos = X}, #grid{rows = Row}) when X < 0 orelse X > Row ->
  io:format("Rover is outside of the grid~n");
get_rover(#rover{y_pos = Y}, #grid{cols = Col}) when Y < 0 orelse Y > Col ->
  io:format("Rover is outside of the grid~n");
get_rover(Rover, _Grid) ->
  Rover.

%%%----------------------------------------------------
%% Rotate rover based on its row position,
%% column position and orientation.
%%%----------------------------------------------------
rotate(#rover{x_pos = X, y_pos = Y, orientation = "N"}, "L") ->
  #rover{x_pos = X, y_pos = Y, orientation = "W"};
rotate(#rover{x_pos = X, y_pos = Y, orientation = "N"}, "R") ->
  #rover{x_pos = X, y_pos = Y, orientation = "E"};
rotate(#rover{x_pos = X, y_pos = Y, orientation = "S"}, "L") ->
  #rover{x_pos = X, y_pos = Y, orientation = "E"};
rotate(#rover{x_pos = X, y_pos = Y, orientation = "S"}, "R") ->
  #rover{x_pos = X, y_pos = Y, orientation = "W"};
rotate(#rover{x_pos = X, y_pos = Y, orientation = "E"}, "L") ->
  #rover{x_pos = X, y_pos = Y, orientation = "N"};
rotate(#rover{x_pos = X, y_pos = Y, orientation = "E"}, "R") ->
  #rover{x_pos = X, y_pos = Y, orientation = "S"};
rotate(#rover{x_pos = X, y_pos = Y, orientation = "W"}, "L") ->
  #rover{x_pos = X, y_pos = Y, orientation = "S"};
rotate(#rover{x_pos = X, y_pos = Y, orientation = "W"}, "R") ->
  #rover{x_pos = X, y_pos = Y, orientation = "N"};
rotate(_Rover, _Rotation) ->
  {error, lost}.

%%%----------------------------------------------------
%% Move the rover forward if it's within grid's bounds;
%% Otherwise, return an error that the rover is "Lost".
%%%----------------------------------------------------
move_forward(Grid, Rover) ->
  UpdatedRover = case forward(Rover) of
    {error, lost} ->
      {error, lost};
    Rover1 ->
      Rover1
  end,

  case is_out_of_bounds(UpdatedRover, Grid) of
    true ->
      {error, lost};
    false ->
      UpdatedRover
  end.

%%%----------------------------------------------------
%%% Validation of input values
%%%----------------------------------------------------
is_grid_valid(#grid{rows = R, cols = C}) when is_integer(R) andalso is_integer(C) ->
  true;
is_grid_valid(_Grid) ->
  false.

is_rover_valid(#rover{x_pos = X, y_pos = Y, orientation = Orientation}) when
  is_integer(X) andalso
  is_integer(Y) andalso
  is_list(Orientation) ->
    true;
is_rover_valid(_Rover) ->
  false.

are_directions_valid(Directions) ->
  lists:all(fun(Direction) -> is_list(Direction) end, Directions).

%%%===================================================================
%% Internal Functions
%%%===================================================================

%% Check whether Rover is within the bounds of the grid.
is_out_of_bounds(#rover{x_pos = X}, _Grid) when X < 0 ->
  true;
is_out_of_bounds(#rover{y_pos = Y}, _Grid) when Y < 0 ->
  true;
is_out_of_bounds(#rover{y_pos = Y}, #grid{cols = ColMax}) when Y > ColMax ->
  true;
is_out_of_bounds(#rover{x_pos = X},#grid{cols = RowMax}) when X > RowMax ->
  true;
is_out_of_bounds(_Rover, _Grid) ->
  false.

%% Move rover based on its row position, column position and orientation.
forward(#rover{x_pos = X, y_pos = Y, orientation = "N"}) ->
  #rover{x_pos = X, y_pos = Y + 1, orientation = "N"};
forward(#rover{x_pos = X, y_pos = Y, orientation = "S"}) ->
  #rover{x_pos = X, y_pos = Y -1, orientation = "S"};
forward(#rover{x_pos = X, y_pos = Y, orientation = "E"}) ->
  #rover{x_pos = X + 1, y_pos = Y, orientation = "E"};
forward(#rover{x_pos = X, y_pos = Y, orientation = "W"}) ->
  #rover{x_pos = X - 1, y_pos = Y, orientation = "W"};
forward(_Rover) ->
  {error, lost}.
