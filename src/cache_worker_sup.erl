-module(cache_worker_sup).
-export([start_link/0, init/1]).
-behaviour(supervisor).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 1, 3600},
          [{cache_worker, {cache_worker, start_link, []}, temporary, 5000, worker, [cache_worker]}]
		 }}.
    
