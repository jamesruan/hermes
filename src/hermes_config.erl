-module(hermes_config).
-include("error_logger.hrl").

-export[start_link/0].

% internal functions
-export[init/1, stop/0, do_save/2, do_save/1, do_save_all/0].

start_link() ->
	Tablist = [{nodes, nodesDumpFile}],
	spawn_link(?MODULE, init, [Tablist]).

load_configs(Name, FileNameKey, Tail) ->
	{ok, File} = application:get_env(FileNameKey),
	case file:open(File, [read, write, exclusive]) of
	{ok, Fd} ->
		?INFO("~p not exist, create for writing.", [File]),
		file:close(Fd),
		Table = ets:new(Name, []),
		put(Name, {Table, File});
	{error, eexist} ->
		?INFO("~p exists, open for writing.", [File]),
		case ets:file2tab(File) of
		{ok, Table} ->
			put(Name, {Table, File});
		{error, Reason} ->
			?ERRORP(Reason),
			case file:open(File, [write]) of
			{ok, Fd} ->
				?INFO("~p broken, create new for writing.", [File]),
				file:close(Fd),
				Table = ets:new(Name, []),
				put(Name, {Table, File});
			{error, Reason} ->
				?ERROR(?PERROR(Reason))
			end
		end
	end,

	case Tail of
	[{NewName, NewFileNameKey} | NewTail] ->
		load_configs(NewName, NewFileNameKey, NewTail);
	_ ->
		ok
	end.

init(Tablist) ->
	put(tablist, Tablist),
	case Tablist of
	[{Name, FileNameKey} | Tail] ->
		ok = load_configs(Name, FileNameKey, Tail)
	end,

	register(hermes_config, self()),
	loop().

stop() ->
	?MODULE:do_save_all(),
	unregister(hermes_config),
	exit(normal).

loop() ->
	receive
	{lookup, Pid, Table, Key} ->
		case get(Table) of
		undefined ->
			?ERROR("Table '~p' not existed.", [Table]);
		{TableId, _File} ->
			case ets:lookup(TableId, Key) of
			[] ->
				Pid ! {ets_lookup, Table, Key, none};
			[{Key, Value}] ->
				Pid ! {ets_lookup, Table, Key, Value}
			end
		end;
	{insert, Pid, Table, Object} ->
		case get(Table) of
		undefined ->
			?ERROR("Table '~p' not existed.", [Table]);
		{TableId, _File} ->
			case Object of 
			{_Key, _Value} ->
				ets:insert(TableId, Object),
				Pid ! {ets_insert, Table, Object, ok};
			_ ->
				?ERROR("Invalid Object: ~p.", [Object])
			end
		end;
	{save, Pid, Table} ->
		Pid ! ?MODULE:do_save(Table);
	{info, Pid, Table} ->
		case get(Table) of
		undefined ->
			?ERROR("Table '~p' not existed.", [Table]);
		{TableId, _File} ->
			Pid ! {ets_info, Table, ets:info(TableId)}
		end;
	stop ->
		?MODULE:stop();
	Other ->
		?WARN("Unknown message: ~p.", [Other])
	end,
	loop().

do_save(Table) ->
	do_save(Table, []).

do_save(Table, Tail) ->
	R = case get(Table) of
	undefined ->
		?ERROR("Table '~p' not existed.", [Table]);
	{TableId, File} ->
		case ets:tab2file(TableId, File) of
		ok ->
			?INFO("Table '~p' saved.", [Table]),
			{ets_save, Table, ok};
		{error, Reason} ->
			?ERROR("Table '~p' can not be saved: ~p.", [Table, Reason]),
			{ets_save, Table, error, Reason}
		end
	end,
	case Tail of
	[{NTable,_}| NTail] ->
		{ets_save, NTable, ok} = do_save(NTable, NTail);
	_ ->
		R
	end.

do_save_all() ->
	Tablelist = get(tablist),
	[{Table,_}| Tail] = Tablelist,
	{ets_save, Table, ok} = do_save(Table, Tail),
	ok.
