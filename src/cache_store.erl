%%
%% This module implements an in memory key-value store with time to live functionality.
%%
%% Its gen_server state fas the following fields:
%%  data = Map[Key]{Value, Pid}. Where:
%%
%%  Key = cache entry key
%%  Value = cache entry value
%%  Pid = Pid of the process responsble for deleting the when frm the cache when it expires.
%%        It needs to be stored because a cache entry can be updated (run store function second time with the same key)
%%        and in this cace we need to cancel the old TTL deletion process and schedule a new one.
%%

-module(cache_store).
-behaviour(gen_server).
-export([start_link/0, store/3, get/1]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2]).


-record(state, {data=maps:new()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store(Key, Value, TTL) ->
    gen_server:call(?MODULE, {store, Key, Value, TTL}).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

%% Gen server callback funnctions
init([]) ->
    {ok, #state{}}.

handle_call({store, Key, Value, TTL}, _From, #state{data=Data}) ->
	cancel_timer(maps:find(Key, Data)),
	Ref = erlang:send_after(TTL, self(), {evict, Key}),
    {reply, ok, #state{data=maps:put(Key, {Value, Ref}, Data)}};
handle_call({get, Key}, _, State=#state{data=Data}) ->
    case maps:find(Key, Data) of
		{ok, {Value, _}} -> 
			{reply, {ok, Value}, State};
		error -> 
			{reply, {error, not_found}, State}
	end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({evict, Key}, State=#state{data=Data}) ->
	{noreply, State#state{data=maps:remove(Key, Data)}}.

terminate(_Reason, _State) ->
    ok.

%% private functions
cancel_timer(error) ->
	ok;
cancel_timer({_, {_, Ref}}) ->
	erlang:cancel_timer(Ref).
