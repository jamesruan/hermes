%%=========================================================
-module(hermes_transport).

-export([start/0, init/1]).

start() ->
	spawn_link(?MODULE, init, ["::1", 2001]).

init(Args) ->
	case Args of
	{AddrStr, Port} ->
		{ok, Address} = inet:parse_ipv6_address(AddrStr)
	end,

	%% Each HTP Node listens on a lower level Socket
	Transport = hermes_transport_udpv6:start(self(), Address, Port),
	put(transport, Transport),

	put(nodes, gb_tree:new()),
	put(connections, gb_tree:new()),

	loop().

loop() ->
	receive
	{packet, SAddress, SPort, Packet} -> 
		case process_packet(SAddress, SPort, Packet) of
		{ok, Key, Data} ->
			case gb_set:is_member(Key, get(connections)) of
			false ->
				OSet = erase(connections),
				NSet = gb_set:add(Key, OSet),
				put(connections, NSet)
			end;
		{error, Reason} ->
			error_logger:error_report(Reason)
		end
	end,
	loop().

process_packet(SAddress, SPort, Packet) ->
	Key = {SAddress, SPort},
	ok.
	
