%% common_test suite for hermes_transport_udpv6

-module(hermes_transport_udpv6_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() -> [{timetrap, {seconds, 10}}].

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() -> [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%
%%      NB: By default, we export all 1-arity user defined functions
%%--------------------------------------------------------------------
all() ->
	[
	test_hermes_transport_udpv6_start,
	test_hermes_transport_udpv6_err_start,
	test_hermes_transport_udpv6_send_receive
	].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% Description: Initialization before each test case group.
%%--------------------------------------------------------------------
init_per_group(_group, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% Description: Cleanup after each test case group.
%%--------------------------------------------------------------------
end_per_group(_group, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(TestCase, Config) ->
    Config.

sleep(Sec) ->
	receive
	M ->
		ct:log("~p ~n", [M]),
		sleep(Sec)
	after Sec*1000 ->
		ok
	end.

test_hermes_transport_udpv6_start() ->
    [{userdata,[{doc,"Testing the hermes_transport_udpv6 module start"}]}
	].

test_hermes_transport_udpv6_start(Config) ->
	AddrStr = ct:get_config(address),
	Port = ct:get_config(port),
	ct:log("~p ~p ~n", [AddrStr, Port]),
	{ok, Addr} = inet:parse_ipv6_address(AddrStr),
	Pid = hermes_transport_udpv6:start(self(), Addr, Port),
	Pid ! stop,
	sleep(1),
	{pass,"Test"}.

test_hermes_transport_udpv6_err_start() ->
    [{userdata,[{doc,"Testing the hermes_transport_udpv6 module error start"}]}
	].

test_hermes_transport_udpv6_err_start(Config) ->
	AddrStr = ct:get_config(address),
	Port = ct:get_config(port),
	ct:log("~p ~p ~n", [AddrStr, Port]),
	{ok, Addr} = inet:parse_ipv6_address(AddrStr),
	Pid = hermes_transport_udpv6:start(self(), Addr, Port),
	PidE = hermes_transport_udpv6:start(self(), Addr, 80),
	Pid ! stop,
	sleep(1),
	{pass,"Test"}.

test_hermes_transport_udpv6_send_receive() ->
    [{userdata,[{doc,"Testing the hermes_transport_udpv6 module send and receive"}]}
	].

test_hermes_transport_udpv6_send_receive(Config) ->
	AddrStr = ct:get_config(address),
	Port = ct:get_config(port),
	ct:log("~p ~p ~n", [AddrStr, Port]),
	{ok, Addr} = inet:parse_ipv6_address(AddrStr),
	PidA = hermes_transport_udpv6:start(self(), Addr, Port),
	PidB = hermes_transport_udpv6:start(self(), Addr, Port+1),
	sleep(1),
	PidA ! {send, self(), Addr, Port+1, <<"test">>},
	sleep(1),
	PidA ! stop,
	PidB ! stop,
	sleep(1),
	{pass,"Test"}.

