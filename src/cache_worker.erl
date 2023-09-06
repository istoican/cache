-module(cache_worker).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
 
start_link(Fn, SendTo) ->
	gen_server:start_link(?MODULE, {Fn, SendTo} , []).

init({Fn, SendTo}) ->
	{ok, {Fn, SendTo}, 0}.

handle_call(_Msg, _From, State) ->
	{noreply, State}.
 
handle_cast(_Msg, State) ->
	{noreply, State}.
 
handle_info(timeout, State={Fn, SendTo}) ->
	case erlang:apply(Fn, []) of
		{error, Reason} ->
			{stop, Reason, State};
		{ok, Value} ->
			SendTo ! {computed, Value, self()},
			{stop, normal, State}
	end.

terminate(_Reason, _State) ->
    ok.
