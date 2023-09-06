# Compiling

```shell
erl -make
```

# Using

```shell
cache:start_link(),
cache:register_function(fun() -> {ok, 1} end, "1", 2000, 20),
cache:register_function(fun() -> {ok, 2} end, "2", 10000, 1000),
cache:register_function(fun() -> {ok, erlang:localtime()} end, "3", 10000, 1000),
cache:register_function(fun() -> timer:sleep(30000), {ok, 4} end, "4", 100000000, 100000),
cache:get("1", 50)
cache:get("2", 500)
cache:get("3", 7000)
cache:get("4", 10)

# Find below the bird view of how the modules are organized:


                               +---------------+
                               |               |
                               |   cache_sup   |
                +------------- | (supervisor)  |----------------+
                |              |               |                |
                |              +---------------+                |
                |                      |                        |         
                v                      v                        v
       +----------------+      +----------------+     +--------------------+
       |                |      |                |     |                    |
       |   cache_serv   |      |  cache_store   |     |  cache_worker_sup  |
       |  (gen_server)  |      |  (gen_server)  |     |    (supervisor)    |
       |                |      |                |     |                    |
       +----------------+      +----------------+     +--------------------+
                                                        |                 |
                                                        |                 | 
                                                        v                 v
                                              +----------------+       +----------------+  
                                              |                |       |                |
                                              |  cache_worker  |       |  cache_worker  |
                                              |  (gen_server)  |  .... |  (gen_server)  |
                                              |                |       |                |
                                              +----------------+       +----------------+  


  * The supervisor module at the top (cache_sup) is there to restart modules below in case of error.

  * cache_serv module job is to schedule the next computation associated with a key and keep track of already registered keys.
    It is also responsble for counting how many workers are curently running, limit that number and maintain a queue of scheduled computation.

  * cache_worker_sup superisor is there to isolate the set of workers from the rest of modules. Also for this sort of 
    dynamic childrens a diferent kind of strategy is needed than the one used by the top supervisor.

  * I chose to separate key-value storage from cache_serv for two main reasons: 
     2. Separating storage from the cache business logic itself makes it easy to swap it in the future.
     1. offload some of cache_serv responsabilities which is already doing too much.
 
  * For the same reasons removing a key from the cache (reguladed by TTL) is cache_store responsability not cache_serv.
  
  * I chose to limit the number of concurrent running computations (running cache_workers) because the size of the cache entries is unbounded.
    This is done inside cache_serv module. 
