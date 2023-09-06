%%
%% # Find below the bird view of how the modules are organized:
%%
%%
%%                               +---------------+
%%                               |               |
%%                               |   cache_sup   |
%%                +------------- | (supervisor)  |----------------+
%%                |              |               |                |
%%                |              +---------------+                |
%%                |                      |                        |         
%%                v                      v                        v
%%       +----------------+      +----------------+     +--------------------+
%%       |                |      |                |     |                    |
%%       |   cache_serv   |      |  cache_store   |     |  cache_worker_sup  |
%%       |  (gen_server)  |      |  (gen_server)  |     |    (supervisor)    |
%%       |                |      |                |     |                    |
%%       +----------------+      +----------------+     +--------------------+
%%                                                        |                 |
%%                                                        |                 | 
%%                                                        v                 v
%%                                              +----------------+       +----------------+  
%%                                              |                |       |                |
%%                                              |  cache_worker  |       |  cache_worker  |
%%                                              |  (gen_server)  |  .... |  (gen_server)  |
%%                                              |                |       |                |
%%                                              +----------------+       +----------------+  
%%
%%
%%  * The supervisor module at the top (cache_sup) is there to restart below modules in case of error.
%%
%%  * cache_serv module job is to schedule the next computation associated with a key and keep track of already registered keys.
%%    It is also responsble for counting how many workers are curently running, limit that number and maintain a queue of scheduled computation.
%%
%%  * cache_worker_sup superisor is there to isolate the set of workers from the rest of modules. Also for this sort of 
%%    dynamic childrens a diferent kind of strategy needed than the one used by the top supervisor.
%%
%%  * I chose to separate key-value storage from cache_serv for two main reasons: 
%%      1. offload some cache_serv responsabilities which is already doing too much.
%%      2. Separating storage from the cache business logic itself makes it easy to swap it in the future.
%% 
%%  * For the same reasons removing a key from the cache (reguladed by TTL) is cache_store responsability not cache_serv.
%% 
%%  * I chose to limit the number of concurrent running computations (running cache_workers) because the size of the cache entries is unbounded,
%%    . This is done inside cache_serv module. 
%%


-module(cache).
-behaviour(application).
-export([start_link/0, register_function/4, get/2]).
-export([start/2, stop/1]).

start_link() ->
	start(normal, []).

start(normal, _Args) ->
    cache_sup:start_link(30).

stop(_State) ->
    ok.

%% Registers a function that will be computed periodically to update the cache.
%%
%% Fn				= fun(() ->)
%% Key 				= term() 
%% TTL 				= timeout()
%% RefreshInterval 	= timeout()
%%
%% Returns: ok | {error, already_registered}
register_function(Fn, Key, TTL, RefreshInterval) ->
	cache_serv:register(Fn, Key, TTL, RefreshInterval).


%% Get the value associated with `key`.
%%
%% Key 		= term()
%% Timeout 	= timeout()
%%
%% Returns: any | {error, timeout} | {error, not_registered}
get(Key, Timeout) ->
	case cache_store:get(Key) of
		{ok, Value} ->
			Value;
		{error, not_found} ->
			cache_serv:get(Key, Timeout)
	end.
    
