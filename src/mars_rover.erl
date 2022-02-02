%%%-------------------------------------------------------------------
%%% @author teodoraardeleanu
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% Creates the grid and the rover;
%%% Moves the rover on the grid by calling different actions based
%%% on the directions provided.
%%% @end
%%% Created : 01. Feb 2022 13:40
%%%-------------------------------------------------------------------
-module(mars_rover).
-author("teodoraardeleanu").

-behaviour(gen_server).

-include("mars_rover.hrl").

%% API
-export([start_link/0, move_rover/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    grid,
    rover,
    directions :: list()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Performs rover actions and returns its final position in the grid.
move_rover() ->
  gen_server:call(?MODULE, move_rover).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server by creating the grid, the rovers and the directions.
init([]) ->
  Grid = setup_grid(),
  Rover = setup_rover(Grid),
  {ok, [Directions]} = file:consult("directions"),
  io:format("~p~n", [lists:flatten(Directions)]),

  {ok, #state{grid = Grid, rover = Rover, directions = Directions}}.

%% @private
%% @doc Handling call messages
handle_call(move_rover, _From, State = #state{grid = Grid, rover = Rover, directions = Directions}) ->
  FinalRoverPosition = handle_actions(Grid, Rover, Directions),
  {reply, FinalRoverPosition, State};
handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
handle_info(_Info, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
terminate(_Reason, _State = #state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Get grid measurements by reading its input values from a file
%% and adding these values to a grid record.
setup_grid() ->
  {ok, [Grid]} = file:consult("grid"),
  {Rows, Columns} = Grid,
  io:format("Grid: ~p ~p~n", [Rows, Columns]),
  mars_rover_actions:get_grid(Grid).

%% Get rover position values by reading its values from an
%% input file and adding these to a rover record.
setup_rover(Grid) ->
  {ok, [{XPos, YPos, Orientation}]} = file:consult("rover"),
  io:format("Rover: (~p,~p,~p) ", [XPos, YPos, Orientation]),
  mars_rover_actions:get_rover(#rover{x_pos = XPos, y_pos = YPos, orientation = Orientation}, Grid).

%% Move rover along the grid by performing rotations and forward movements.
%%  - if the rover passes the grid bounds, we print out the final valid directions;
%%  - Once all directions in the list are checked, its final position is printed out.
handle_actions(_Grid, #rover{x_pos = X, y_pos = Y, orientation = Orientation}, []) ->
  io:format("Rover final position: (~p,~p,~p)~n", [X, Y, Orientation]);
%% Rotate the rover by checking the direction from the input file is
%% either left ("L") or right ("R");
handle_actions(Grid, Rover =  #rover{x_pos = X, y_pos = Y, orientation = Orientation}, [Dir | Directions]) when Dir == "L" orelse Dir == "R" ->
  case mars_rover_actions:rotate(Rover, Dir) of
    {error, lost} ->
      io:format("(~p,~p,~p) LOST~n", [X, Y, Orientation]);
    UpdatedRover ->
      handle_actions(Grid, UpdatedRover, Directions)
  end;
%% Move the rover forward by checking the direction from the input file ("F");
handle_actions(Grid, Rover = #rover{x_pos = X, y_pos = Y, orientation = Orientation}, [Dir | Directions]) when Dir == "F" ->
  case mars_rover_actions:move_forward(Grid, Rover) of
    {error, lost} ->
      io:format("(~p,~p,~p) LOST~n", [X, Y, Orientation]);
    UpdatedRover ->
       handle_actions(Grid, UpdatedRover, Directions)
  end;
handle_actions(Grid, Rover, Directions) ->
  handle_actions(Grid, Rover, Directions).
