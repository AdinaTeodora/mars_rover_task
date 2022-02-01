%%%-------------------------------------------------------------------
%% @doc mars_rover public API
%% @end
%%%-------------------------------------------------------------------
-module(mars_rover_app).

-behaviour(application).

-export([start/2, stop/1]).

start(normal, []) ->
    io:format("Started Mars Rover application~n"),
    mars_rover_sup:start_link().

stop(_State) ->
    ok.