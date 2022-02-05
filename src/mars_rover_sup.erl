%%%-------------------------------------------------------------------
%% @doc mars_rover top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(mars_rover_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(MARS_ROVER_SERVER, mars_rover).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
      strategy  => one_for_one,
      intensity => 100,
      period    => 5
    },

    ChildSpecs = [#{
      id       => ?MARS_ROVER_SERVER,
      start    => {?MARS_ROVER_SERVER, start_link, []},
      restart  => permanent,
      shutdown => 2000,
      type     => worker,
      modules  => [?MARS_ROVER_SERVER]
    }],

    {ok, {SupFlags, ChildSpecs}}.

