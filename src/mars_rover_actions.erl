%%%-------------------------------------------------------------------
%%% @author teodoraardeleanu
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Jan 2022 18:41
%%%-------------------------------------------------------------------
-module(mars_rover_actions).
-author("teodoraardeleanu").

-include("mars_rover.hrl").

%% API
-export([create_grid/2, get_grid/1]).
-export([create_rover/2, rotate/2, forward/2]).

create_grid(M, N) when is_integer(M) andalso is_integer(N) ->
  io:format("~p ~p~n", [M, N]),
  Grid = [{Row * 1, Col * 1} || Row <- lists:seq(0, M), Col <- lists:seq(0, N)],
  {ok, Grid}.

get_grid(Grid) ->
  {Rows, Cols} = lists:last(Grid),
  #grid{rows = Rows, cols = Cols}.

create_rover(#rover{x_pos = X}, #grid{rows = Row}) when X < 0 orelse X > Row ->
  io:format("Rover is outside of the grid");
create_rover(#rover{y_pos = Y}, #grid{cols = Col}) when Y < 0 orelse Y > Col ->
  io:format("Rover is outside of the grid");
create_rover(Rover, _Grid) ->
  Rover.

rotate(#rover{orientation = "N"}, "L") ->
  #rover{orientation = "W"};
rotate(#rover{orientation = "N"}, "R") ->
  #rover{orientation = "E"};
rotate(#rover{orientation = "S"}, "L") ->
  #rover{orientation = "E"};
rotate(#rover{orientation = "S"}, "R") ->
  #rover{orientation = "W"};
rotate(#rover{orientation = "E"}, "L") ->
  #rover{orientation = "N"};
rotate(#rover{orientation = "E"}, "R") ->
  #rover{orientation = "S"};
rotate(#rover{orientation = "W"}, "L") ->
  #rover{orientation = "S"};
rotate(#rover{orientation = "W"}, "R") ->
  #rover{orientation = "N"};
rotate(_Rover, _Rotation) ->
  io:format("Unknown rotation").

forward(#rover{x_pos = X, y_pos = Y, orientation = Orientation}, #grid{rows = R, cols = C}) ->
  case Orientation of
    "N" ->
      case Y =/= R of
        true ->
          #rover{y_pos = Y + 1};
        false ->
          io:format("Cannot move outside of grid")
      end;
    "S" ->
      case Y =/= 0 of
        true ->
          #rover{y_pos = Y - 1};
        false ->
          io:format("Cannot move outside of grid")
      end;
    "E" ->
      case X =/= C of
        true ->
          #rover{x_pos = X + 1};
        false ->
          io:format("Cannot move outside of grid")
      end;
    "W" ->
      case Y =/= R of
        true ->
          #rover{x_pos = X - 1};
        false ->
          io:format("Cannot move outside of grid")
      end;
    _UnknownDirection ->
      io:format("Unknown direction")
  end.