{application, cache,
 [{vsn, "1.0.0"},
  {modules, [cache, cache_serv, cache_sup, cache_store, cache_worker_sup, cache_worker]},
  {registered, [cache]},
  {mod, {cache, []}}
 ]}.
