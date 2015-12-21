-module(hermes_config).

-export[start_link/0].

% internal functions
-export[init/1, stop/0, do_save/1].

start_link() ->
	{ok, NodesDump} = application:get_env(nodesDumpFile),
	spawn_link(?MODULE, init, [NodesDump]).

init(NodesDump) ->
	case file:open(NodesDump, [read, write, exclusive]) of
	{ok, Fd} ->
		error_logger:info_msg("NodesDumpFile not exist, create for writing.~n"),
		file:close(Fd),
		Nodes = ets:new(nodes, []),
		put(nodes, {Nodes, NodesDump});
	{error, eexist} ->
		error_logger:info_msg("NodesDumpFile exists, open for writing.~n"),
		case ets:file2tab(NodesDump) of
		{ok, Nodes} ->
			put(nodes, {Nodes, NodesDump});
		{error, Reason} ->
			error_logger:error_report(Reason),
			exit(terminated)
		end;
	{error, Reason} ->
		error_logger:error_report(Reason),
		exit(terminated)
	end,

	register(hermes_config, self()),
	loop().

stop() ->
	{ets_save, nodes, ok} = ?MODULE:do_save(nodes),
	unregister(hermes_config),
	exit(normal).

loop() ->
	receive
	{lookup, Pid, Table, Key} ->
		case get(Table) of
		undefined ->
			error_logger:error_msg("Table '~p' not existed.~n", [Table]);
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
			error_logger:error_msg("Table '~p' not existed.~n", [Table]);
		{TableId, _File} ->
			case Object of 
			{_Key, _Value} ->
				ets:insert(TableId, Object),
				Pid ! {ets_insert, Table, Object, ok};
			_ ->
				error_logger:warning_msg("Invalid Object: ~p ~n.", [Object])
			end
		end;
	{save, Pid, Table} ->
		Pid ! ?MODULE:do_save(Table);
	{info, Pid, Table} ->
		case get(Table) of
		undefined ->
			error_logger:error_msg("Table '~p' not existed.~n", [Table]);
		{TableId, _File} ->
			Pid ! {ets_info, Table, ets:info(TableId)}
		end;
	stop ->
		?MODULE:stop();
	Other ->
		error_logger:warn_msg("Unknown message: ~p.~n", [Other])
	end,
	loop().

do_save(Table) ->
	case get(Table) of
	undefined ->
		error_logger:error_msg("Table '~p' not existed.~n", [Table]);
	{TableId, File} ->
		case ets:tab2file(TableId, File) of
		ok ->
			error_logger:info_msg("Table '~p' saved.~n", [Table]),
			{ets_save, Table, ok};
		{error, Reason} ->
			error_logger:error_msg("Table '~p' can not be saved: ~p.~n", [Table, Reason]),
			{ets_save, Table, error, Reason}
		end
	end.
