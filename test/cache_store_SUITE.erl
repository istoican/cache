-module(cache_store_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_testcase/2]).
-export([match/1, eviction/1, unmatch/1]).
 
all() -> [match, eviction, unmatch].

init_per_testcase(_, Config) ->
	cache_store:start_link(),
	Config.

match(_Config) ->
	ok = cache_store:store("key1", "v", 500),
	{ok, "v"} = cache_store:get("key1").

unmatch(_Config) ->
	{error, not_found} = cache_store:get("key2").

eviction(_Config) ->
	ok = cache_store:store("key1", "", 500),
	{ok, ""} = cache_store:get("key1"),
	timer:sleep(1000),
	{error, not_found} = cache_store:get("key1").
