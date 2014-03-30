-module(fcache).

-export([start/0,stop/0, create/2, drop/1, put/3, get/2, remove/2]).

start()-> application:start(fcache).
stop()-> application:stop(fcache).

create(Name, Opts) -> 
	{ok, _Pid} = supervisor:start_child(fcache_herder, [Name, Opts]),
	{ok, Name}.

drop(Name) -> supervisor:terminate_child(fcache_herder, whereis(Name)).

put(Cache, Key, Value)-> gen_server:cast(Cache, {put, {Key, Value}}).
get(Cache, Key)->gen_server:call(Cache, {get, Key}).
remove(Cache, Key)-> gen_server:cast(Cache, {del, Key}).
