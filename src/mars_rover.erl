%%%-------------------------------------------------------------------
%%% @author teodoraardeleanu
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2022 13:40
%%%-------------------------------------------------------------------
-module(mars_rover).
-author("teodoraardeleanu").

-behaviour(gen_server).

-include("mars_rover.hrl").

%% API
-export([start_link/0, manage_rover/0]).

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

manage_rover() ->
  gen_server:call(?MODULE, manage_rover).

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
handle_call(manage_rover, _From, State = #state{grid = Grid, rover = Rover, directions = Directions}) ->
  FinalRover = handle_actions(Grid, Rover, Directions),
  {reply, FinalRover, State};
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

setup_grid() ->
  {ok, [Grid]} = file:consult("grid"),
  {Rows, Columns} = Grid,
  io:format("Grid: ~p ~p~n", [Rows, Columns]),
  mars_rover_actions:get_grid(Grid).

setup_rover(Grid) ->
  {ok, [{XPos, YPos, Orientation}]} = file:consult("rover"),
  io:format("Rover: (~p,~p,~p) ", [XPos, YPos, Orientation]),
  mars_rover_actions:get_rover(#rover{x_pos = XPos, y_pos = YPos, orientation = Orientation}, Grid).

handle_actions(_Grid, #rover{x_pos = X, y_pos = Y, orientation = Orientation}, []) ->
  io:format("Rover final position: (~p,~p,~p)~n", [X, Y, Orientation]);
handle_actions(Grid, Rover, [Dir | Directions]) when Dir == "L" orelse Dir == "R" ->
  UpdatedRover = mars_rover_actions:rotate(Rover, Dir),
  handle_actions(Grid, UpdatedRover, Directions);
handle_actions(Grid, Rover, [Dir | Directions]) when Dir == "F" ->
  UpdatedRover = mars_rover_actions:forward(Rover),
  handle_actions(Grid, UpdatedRover, Directions);
handle_actions(Grid, Rover, Directions) ->
  handle_actions(Grid, Rover, Directions).