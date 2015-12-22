-module(hermes_config).
-behaviour(gen_server).
-include("error_logger.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Callback Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Internal Function Exports
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% API Function Definition
%% ------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Callback Function Definitions
%% ------------------------------------------------------------------

init([]) ->
	process_flag(trap_exit, true),

	{ok, Tablelist} = application:get_env(tablelist),
	TableFileList = lists:map(fun(TableName) ->
		{ok, File} = application:get_env(hermes, TableName),
		{TableName, File} end,
		 Tablelist),

	TableFileTabList = [load_configs(X) || X <- TableFileList],
	{ok, TableFileTabList}.

terminate(Reason, State) ->
	case Reason of
	normal ->
		?INFO("hermes_config exit normally."),
		do_save_all(State);
	shutdown ->
		?INFO("hermes_config terminated."),
		do_save_all(State);
	{shutdown, Info} ->
		?INFO("hermes_config terminated with info: ~p.", [Info]),
		do_save_all(State);
	OtherReason ->
		?INFO("hermes_config was terminating by reason:~p.", [OtherReason])
	end,
	ok.

handle_call(Request, _From, State) ->
	TableFileTabList = State,
	case Request of
	{lookup, Table, Key} ->
		case lists:keyfind(Table, 1, TableFileTabList) of
		false ->
			?ERROR("Table '~p' not existed.", [Table]),
			{reply, {ets_lookup, Table, Key, error}, State};
		{Table, _File, Tab} ->
			case ets:lookup(Tab, Key) of
			[] ->
				{reply, {ets_lookup, Table, Key, none}, State};
			[{Key, Value}] ->
				{reply, {ets_lookup, Table, Key, Value}, State}
			end
		end;
	{insert, Table, Object} ->
		case lists:keyfind(Table, 1, TableFileTabList) of
		false ->
			?ERROR("Table '~p' not existed.", [Table]),
			{reply, {ets_insert, Table, Object, error}, State};
		{Table, _File, Tab} ->
			case Object of
			{_Key, _Value} ->
				ets:insert(Tab, Object),
				{reply, {ets_insert, Table, Object, ok}, State};
			_ ->
				?ERROR("Invalid Object: ~p.", [Object]),
				{reply, {ets_insert, Table, Object, invalid}, State}
			end
		end;
	{save, Table} ->
		case lists:keyfind(Table, 1, TableFileTabList) of
		false ->
			?ERROR("Table '~p' not existed.", [Table]),
			{reply, {ets_save, Table, error}, State};
		TableFileTab ->
			{reply, {ets_save, Table, do_save(TableFileTab)}, State}
		end;
	{info, Table} ->
		case lists:keyfind(Table, 1, TableFileTabList) of
		false ->
			?ERROR("Table '~p' not existed.", [Table]),
			{reply, {ets_info, Table, error}, State};
		{Table, _File, Tab} ->
			{reply, {ets_info, Table, ets:info(Tab)}, State}
		end;
	Other ->
		?WARN("Unknown message: ~p.", [Other]),
		{reply, unknown, State}
	end.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Functions
%% ------------------------------------------------------------------
do_save(TableFileTab) ->
	{TableName, File, Tab} = TableFileTab,
	ok = case ets:tab2file(Tab, File) of
	ok ->
		?INFO("Table '~p' saved.", [TableName]),
		ok;
	{error, Reason} ->
		?ERROR("Table '~p' can not be saved: ~p.", [TableName, Reason]),
		error
	end.

do_save_all(TableFileTabList) ->
	[ok = do_save(X) || X <- TableFileTabList],
	ok.

load_configs(TableFile) ->
	?INFOP(TableFile),
	{TableName, FilePath} = TableFile,

	case open_create(FilePath) of
	created ->
		Tab = ets:new(TableName, []),
		{TableName, FilePath, Tab};
	existed ->
		?INFO("~p exists, loading.", [FilePath]),
		case ets:file2tab(FilePath) of
		{ok, Tab} ->
			{TableName, FilePath, Tab};
		{error, Reason} ->
			?WARNP(Reason),
			?INFO("~p broken, create new for writing.", [FilePath]),
			ok = open_truncate(FilePath),
			Tab = ets:new(TableName, []),
			{TableName, FilePath, Tab}
		end
	end.

open_create(FilePath) ->
	case file:open(FilePath, [read, write, exclusive]) of
	{ok, File} ->
		?INFO("~p not exist, creating.", [FilePath]),
		file:close(File),
		created;
	{error, eexist} ->
		existed
	end.

open_truncate(FilePath) ->
	case file:open(FilePath, [write]) of
	{ok, File} ->
		file:close(File),
		ok;
	{error, Reason} ->
		?ERROR(?PERROR(Reason)),
		error
	end.
