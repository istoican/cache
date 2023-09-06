-module(cache_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_testcase/2]).
-export([not_registered_key/1, simple_get/1, timeout_get/1, already_registered/1, simple_register/1]).
 
all() -> [not_registered_key, simple_get, timeout_get, already_registered].

init_per_testcase(_, Config) ->
	cache:start_link(),
	Config.

not_registered_key(_Config) ->
	{error, not_registered} = cache:get("k1", 500).

simple_get(_Config) ->
	ok = cache:register_function(fun() -> {ok, "v"} end, "k1", 5000, 500),
	"v" = cache:get("k1", 5).

timeout_get(_Config) ->
	ok = cache:register_function(fun() -> timer:sleep(1000), {ok, "v"} end, "k1", 5000, 500),
	{error, timeout} = cache:get("k1", 10).

simple_register(_Config) ->
	ok = cache:register_function(fun() -> timer:sleep(1000), {ok, "v"} end, "k1", 5000, 500).

already_registered(_Config) ->
	ok = cache:register_function(fun() -> {ok, "v"} end, "k1", 5000, 500),
	{error, already_registered} = cache:register_function(fun() -> "v" end, "k1", 5000, 500).
