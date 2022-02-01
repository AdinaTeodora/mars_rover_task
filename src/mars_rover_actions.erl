%%%-------------------------------------------------------------------
%%% @author teodoraardeleanu
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% Contains grid handling functions and rover actions.
%%% @end
%%% Created : 31. Jan 2022 18:41
%%%-------------------------------------------------------------------
-module(mars_rover_actions).
-author("teodoraardeleanu").

-include("mars_rover.hrl").

%% API

%% Grid functionality
-export([get_grid/1]).
%% Rover functionality
-export([get_rover/2, rotate/2, forward/1]).

get_grid({M, N}) ->
  #grid{rows = M - 1, cols = N - 1}.

get_rover(#rover{x_pos = X}, #grid{rows = Row}) when X < 0 orelse X > Row ->
  io:format("Rover is outside of the grid~n");
get_rover(#rover{y_pos = Y}, #grid{cols = Col}) when Y < 0 orelse Y > Col ->
  io:format("Rover is outside of the grid~n");
get_rover(Rover, _Grid) ->
  Rover.

%% Rotating ability for the rover based on the following:
%% Row Position, Column Position and Orientation of the
%% Rover on the Grid.
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
  io:format("Unknown commands ~n").

%%check_bounds(#rover{x_pos = X, y_pos = Y}, ) ->
%%  .

%% Moving ability for the rover based on the following:
%% Row Position, Column Position and Orientation of the
%% Rover, and the Grid coordinates. %% TODO
forward(#rover{x_pos = X, y_pos = Y, orientation = "N"}) ->
  #rover{x_pos = X + 1, y_pos = Y, orientation = "N"};
forward(#rover{x_pos = X, y_pos = Y, orientation = "S"}) ->
  #rover{x_pos = X - 1, y_pos = Y, orientation = "S"};
forward(#rover{x_pos = X, y_pos = Y, orientation = "E"}) ->
  #rover{x_pos = X, y_pos = Y + 1, orientation = "E"};
forward(#rover{x_pos = X, y_pos = Y, orientation = "W"}) ->
  #rover{x_pos = X, y_pos = Y - 1, orientation = "W"};
forward(_Rover) ->
  io:format("Off the grid~n").