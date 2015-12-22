-module(hermes_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), #{
	id => I,
	start => {I, start_link, []},
	restart => permanent,
	shutdown => 5000,
	type => Type,
	modules => [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link([]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    init({one_for_one, 1, 5});
init({Strategy, MaxRestart, MaxTime}) ->
	SupFlags = #{strategy => Strategy, intensity => MaxRestart, period => MaxTime},
	ChildSpecs = [?CHILD(hermes_config, worker)
		],
	{ok, {SupFlags, ChildSpecs}}.

