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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-export([manage_rover/3]).

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

manage_rover(Grid, Rover, Directions) ->
    gen_server:call(?SERVER, {manage_rover, Grid, Rover, Directions}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server by creating the grid, the rovers and the directions.
init([]) ->
    Grid = setup_grid(),
    io:format("Grid ~p~n", [Grid]),
    Rover = setup_rover(Grid),
    io:format("Rover ~p~n", [Rover]),
    {ok, [Directions]} = file:consult("directions"),
    io:format("Directions ~p~n", [Directions]),

    handle_actions(Grid, Rover, Directions),
    {ok, #state{grid = Grid, rover = Rover, directions = Directions}}.

%% @private
%% @doc Handling call messages
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
    {ok, GridCreated} = mars_rover_actions:create_grid(Grid),
    mars_rover_actions:get_grid_boundaries(GridCreated).

setup_rover(Grid) ->
    {ok, [{XPos, YPos, Orientation}]} = file:consult("rover"),
    mars_rover_actions:create_rover(#rover{x_pos = XPos, y_pos = YPos, orientation = Orientation}, Grid).

handle_actions(_Grid, Rover, []) ->
    io:format("rover's position is: (~p, ~p, ~p)~n", [Rover#rover.x_pos, Rover#rover.y_pos, Rover#rover.orientation]);
handle_actions(Grid, Rover, [Dir | Directions]) when Dir == "L" orelse Dir == "R" ->
    UpdatedRover = mars_rover_actions:rotate(Rover, Dir),
    handle_actions(Grid, UpdatedRover, Directions);
handle_actions(Grid, Rover, [Dir | Directions]) when Dir == "F" ->
    UpdatedRover = mars_rover_actions:forward(Rover, Grid),
    handle_actions(Grid, UpdatedRover, Directions);
handle_actions(Grid, Rover, Directions) ->
    handle_actions(Grid, Rover, Directions).