-module(cache_sup).
-export([start_link/1, init/1]).
-behaviour(supervisor).

start_link(Limit) ->
    supervisor:start_link({local, cache}, ?MODULE, {Limit}).

init({Limit}) ->
    {ok, {{one_for_all, 2, 3600},
          [{server, {cache_serv, start_link, [Limit, self()]}, permanent, 5000, worker, [cache_serv]},
	       {store, {cache_store, start_link, []}, permanent, 5000, worker, [cache_store]}
		  ]
		 }
	}.

