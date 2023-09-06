%%
%% This module is responsible for:
%%   * limiting how many computations(workers) are running at once.
%%   * Schedule next value compution based on RefreshInterval.
%%   * Keep track of registerd keys.
%% 
%% It's state fields are:
%%   * limit = maximum number of workers that can be started at a kiven time.It is always >=0.
%%   * sup   = Pid of worker pool supervisor
%%   * keys  = list of registered keys
%%   * refs  = List of running workers. 
%%             It has the followng signature: [{Pid, {Blueprint, Listeners}}]. Where:
%%             Pid = represend the Pid of running worker
%%             Listeners = List of awaiting clients for that pid computations
%%			   Blueprint = Tuple: {Fn, Key, TTL, RefreshInterval} .
%%   * queue = List of awaiting computations to be run.
%%

-module(cache_serv).
-behaviour(gen_server).
-export([start_link/2, register/4, get/2]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2]).


%% Gen server state
 -record(state, {limit=0,				% maximum number of concurrent workers
				 sup,					% Pid of workers_supervisor
				 keys=[],				% List of already registered keys.
            	 refs=dict:new(),		% Keeps track of running workers
				 queue=queue:new()		% Keeps track of queued computations to be executed.
				}).

%% Module API

%%
%%	Limit	- Max number of workers
%%  Sup 	- Pid of process supervisor. It is used to register the worker_supervisor under the same supervisor tree.
%%
%%  Returns
start_link(Limit, Sup) when is_integer(Limit) ->
    gen_server:start({local, ?MODULE}, ?MODULE, {Limit, Sup}, []).

%% 
%% Registers a function that will be computed periodically to update the cache.
%%
%% Returns: ok | {error, already_registered}
register(Fn, Key, TTL, RefreshInterval) ->
    gen_server:call(?MODULE, {register, {Fn, Key, TTL, RefreshInterval}}).

%% Get the value associated with `key`.
%%
%% Returns: any | {error, timeout} | {error, not_registered}
get(Key, Timeout) ->
	try gen_server:call(?MODULE, {get, Key}, Timeout) of
		not_found -> 
			{error, not_registered};
		{ok, Value} ->
			Value
	catch
		exit:{timeout, _} ->
			{error, timeout}
	end.

%% gen_server callbacks
init({Limit, Sup}) ->
    self() ! {start_worker_supervisor, Sup},
    {ok, #state{limit=Limit}}.

handle_call({get, Key}, From, S = #state{refs=Refs}) ->
	case dict:fold(fun(Pid, {{_, K, _, _}, _}, Acc) -> if K =:= Key -> Pid; true -> Acc end end, not_found, Refs) of
		not_found ->
			{reply, not_found, S};
		Pid -> 
			S2 = S#state{refs=dict:update(Pid, fun({A, L}) -> {A, [From | L]} end, Refs)},
			{noreply, S2}
	end;
handle_call({register, R={_Fn, Key, _TTL, _RefreshInterval}}, _From, S=#state{keys=L}) ->
	case lists:member(Key, L) of
		true ->
			{reply, {error, already_registered}, S};
		false ->
			erlang:send(self(), {refresh, R}),
			{reply, ok, S#state{keys=[Key|L]}}
	end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _}, S = #state{refs=Refs}) ->
	case dict:take(Pid, Refs) of
        error ->
            {noreply, S};   	% Do nothing. It is not a worker process. 
		{{Args={_Fn, _Key, _TTL, RefreshInterval}, _}, Refs2} ->
			erlang:send_after(RefreshInterval, self(), {refresh, Args}),
			run_next_in_queue(S#state{refs=Refs2})
	end;
handle_info({start_worker_supervisor, Sup}, S = #state{}) ->
	Spec = {
	 	worker_sup,
			{cache_worker_sup, start_link, []},
			temporary,
			10000,
			supervisor,
			[cache_worker_sup]
	 },
    {ok, Pid} = supervisor:start_child(Sup, Spec),
    link(Pid),
    {noreply, S#state{sup=Pid}};
handle_info({computed, Result, Ref}, S=#state{refs=Refs}) ->
	case dict:find(Ref, Refs) of
		error ->
			{noreply, S};
		{ok, {{_Fn, Key, TTL, _}, L}} ->
			cache_store:store(Key, Result, TTL),
			notify_listeners(Result, L),
			{noreply, S};
		V ->
			erlang:display(V),
			{noreply, S}
	end;
handle_info({refresh, Args={Fn, _Key, _TTL, _RefreshInterval}}, S = #state{sup=Sup, limit=N, refs=Refs}) when N > 0 ->
	{ok, Pid} = supervisor:start_child(Sup, [Fn, self()]),
	erlang:monitor(process, Pid),
	{noreply, S#state{limit=N-1, refs=dict:store(Pid, {Args, []}, Refs)}};
handle_info({refresh, Args}, S = #state{queue=Q, limit=N}) when N  =< 0 ->
	{noreply, S#state{queue=queue:in(Q, Args)}};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

notify_listeners(_Result, []) ->
	ok;
notify_listeners(Result, [From|Refs]) ->
	gen_server:reply(From, {ok, Result}),
	notify_listeners(Result, Refs).

run_next_in_queue(S = #state{limit=N, queue=Q, sup=Sup, refs=Refs}) ->
    case queue:out(Q) of
        {Args={Fn, _Key, _TTL, _RefreshInterval}, Q2} ->
            {ok, Pid} = supervisor:start_child(Sup, [Fn, self()]),
            erlang:monitor(process, Pid),
            {noreply, S#state{queue=Q2, refs=dict:store(Pid, {Args, []}, Refs)}};
        {empty, _} ->
            {noreply, S#state{limit=N+1}}
    end.
