-module(fcache_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_table/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_table(Name) when is_atom(Name) -> 
	gen_server:cast(?SERVER, {get_table, Name}),
	receive 
		{'ETS-TRANSFER', Tab, _FromPid, fresh} -> {ok, {fresh, Tab}};
		{'ETS-TRANSFER', Tab, _FromPid, stale} -> {ok, {stale, Tab}}
	after 
		10000 -> timeout
	end.
		
		

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({get_table, Name}, State) ->
	case ets:info(Name) of
		undefined ->
			Table = ets:new(Name, [named_table, {heir, self(), Name}]),
			ets:give_away(Table, whereis(Name), fresh);
		_ ->
			ets:give_away(Name, whereis(Name), stale)
	end,
	{noreply, State};
	
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

