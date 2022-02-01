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
-export([create_grid/1, get_grid_boundaries/1]).
%% Rover functionality
-export([create_rover/2, rotate/2, forward/2]).

create_grid({M, N}) when is_integer(M) andalso is_integer(N) ->
  [{Row * 1, Col * 1} || Row <- lists:seq(0, M), Col <- lists:seq(0, N)].

get_grid_boundaries(Grid) ->
  {Rows, Cols} = lists:last(Grid),
  #grid{rows = Rows, cols = Cols}.

create_rover(#rover{x_pos = X}, #grid{rows = Row}) when X < 0 orelse X > Row ->
  io:format("Rover is outside of the grid~n");
create_rover(#rover{y_pos = Y}, #grid{cols = Col}) when Y < 0 orelse Y > Col ->
  io:format("Rover is outside of the grid~n");
create_rover(Rover, _Grid) ->
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

%% Moving ability for the rover based on the following:
%% Row Position, Column Position and Orientation of the
%% Rover, and the Grid coordinates. %% TODO
forward(#rover{x_pos = X, y_pos = Y, orientation = Orientation}, #grid{rows = R, cols = C}) ->
  case Orientation of
    "N" ->
      case Y =/= R of
        true ->
          #rover{y_pos = Y + 1};
        false ->
          io:format("Cannot move outside of grid~n")
      end;
    "S" ->
      case Y =/= 0 of
        true ->
          #rover{y_pos = Y - 1};
        false ->
          io:format("Cannot move outside of grid~n")
      end;
    "E" ->
      case X =/= C of
        true ->
          #rover{x_pos = X + 1};
        false ->
          io:format("Cannot move outside of grid~n")
      end;
    "W" ->
      case Y =/= R of
        true ->
          #rover{x_pos = X - 1};
        false ->
          io:format("Cannot move outside of grid~n")
      end;
    _UnknownDirection ->
      io:format("Unknown direction~n")
  end.