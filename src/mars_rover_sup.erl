%%%-------------------------------------------------------------------
%% @doc mars_rover top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(mars_rover_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
      strategy  => one_for_one,
      intensity => 100,
      period    => 5
    },

    ChildSpecs = [#{
      id       => mars_rover,
      start    => {mars_rover, start_link, []},
      restart  => permanent,
      shutdown => 2000,
      type     => worker,
      modules  => [mars_rover]
    }],

    {ok, {SupFlags, ChildSpecs}}.

