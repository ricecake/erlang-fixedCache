-module(fcache_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(cache, {table, filter, size=1000, variance=250, activity=10, probability=0.001}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name, Opts) when is_list(Opts)->
    gen_server:start_link({local, Name}, ?MODULE, {Name, Opts}, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Name, Opts}) ->
	Size  = proplists:get_value(size, Opts, 1000),
	Var   = proplists:get_value(variance,   Opts, 250),
	Act   = proplists:get_value(activity,   Opts, 10),
	Prob  = proplists:get_value(probabilty, Opts, 0.001),
	{ok, Bloom} = ebloom:new(Act*Size, Prob, random:uniform(10000)),
	case fcache_srv:get_table(Name) of 
		{ok, {fresh, Table}} -> {ok, #cache{table=Table, filter=Bloom, size=Size, variance=Var, activity=Act, probability=Prob}};
		{ok, {stale, Table}} -> {ok, #cache{table=Table, filter=Bloom, size=Size, variance=Var, activity=Act, probability=Prob}}
	end.


handle_call({get, Key}, _From, #cache{table=Table, filter=Bloom} = State) -> 
	case ebloom:contains(Bloom, Key) of
		false -> {reply, {ok, undef}, State};
		true  ->
			[{Key, _Counter, Value}] = ets:lookup(Table,Key),
			ets:update_counter(Table, Key, 1),
			{reply, {ok, {Key, Value}}, State}
	end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({put, {Key, Value}}, #cache{table=Table, filter=Bloom, size=Size, variance=Var, activity=Act, probability=Prob} = State) ->
	case ets:insert_new(Table, {Key, 0, Value}) of
		false -> ets:update_counter(Table, Key, 1), ets:update_element(Table, Key, {3, Value});
		true  -> ebloom:insert(Bloom, Key)
	end,
	ActualSize = ets:info(Table, size),
	case ActualSize > Size+Var of
		true -> 
			Filter = pruneTable(Table, ActualSize, Size, Var, Act, Prob),
			{noreply, State#cache{filter=Filter}};
		false-> {noreply, State}
	end;
handle_cast({del, Key}, #cache{table=Table} = State) -> 
	true = ets:delete(Table, Key),
	{noreply, State};
handle_cast({new_bloom, NewBloom}, State) -> {noreply, State#cache{filter=NewBloom}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

pruneTable(Table, ActualSize, Size, Var, Act, Prob) when ActualSize > Size+Var -> 
	io:format("~p~n",[[Table,ActualSize, Size,Var]]),
	Prune = ActualSize-(Size-Var),
	DeleteList = ets:foldl(fun({Key, Count, _Value}, Acc)-> lists:sublist(lists:merge(Acc, [{Count, Key}]), Prune) end, [], Table),
	[ets:delete(Table, Key) || {_Count, Key} <- DeleteList],
	{ok, Bloom} = ebloom:new(Act*Size, Prob, random:uniform(10000)),
	NewFilter = ets:foldl(fun({Key, _Count, _Val}, Acc) -> ebloom:insert(Acc, Key), Acc end, Bloom, Table),
	NewFilter.
