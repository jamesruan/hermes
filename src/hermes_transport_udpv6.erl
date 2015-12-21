-module('hermes_transport_udpv6').
-include("error_logger.hrl").

-export([start/3, init/3]).

start(Pid, Address, Port) ->
	process_flag(trap_exit, true),
	spawn_link(?MODULE, init, [Pid, Address, Port]).

init(Pid, Address, Port) ->
	case gen_udp:open(Port, [binary, {ip, Address}, inet6, {ipv6_v6only, true}, {deliver, term}]) of
	{ok, Socket} ->
		put(receiver, Pid),
		put(socket, Socket),
		put(name, inet:sockname(Socket)),
		loop();
	{error, Reason} ->
		?ERRORP(Reason),
		exit(error)
	end.

loop() ->
	Socket = get(socket),
	receive
	stop ->
		gen_udp:close(Socket),
		exit(normal);
	{udp, Socket, SAddress, SPort, Data} ->
		%% All incomming packet is sent to get(receiver).
		get(receiver) ! {packet, {SAddress, SPort, Data}},
		loop();
	{send, Pid, DAddress, DPort, Data} ->
		case gen_udp:send(Socket, DAddress, DPort, Data) of
		%% Pids sent packets get a reply.
		ok ->
			Pid ! {ok, self()};
		{error, Reason} ->
			Pid ! {error, self(), Reason}
		end,
		loop();
	{'EXIT', _Port, Reason} ->
		?INFO("Socket ~p closed:~p.", [get(name), Reason]),
		exit(Reason)
	end.
