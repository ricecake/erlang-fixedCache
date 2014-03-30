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

-record(cache, {table, size=1000, variance=250, filter}).

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
		{ok, {fresh, Table}} -> {ok, #cache{table=Table, filter=Bloom, size=Size, variance=Var}};
		{ok, {stale, Table}} -> {ok, #cache{table=Table, filter=Bloom, size=Size, variance=Var}}
	end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

