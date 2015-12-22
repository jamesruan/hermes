-module(hermes_app).

-behaviour(application).
-include("error_logger.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	%% Start error_logger
	{ok, LogFile} = application:get_env(errorLoggerFile),
	case error_logger:logfile({open, LogFile}) of
		ok ->
			?INFO("File Logger started~n");
		{error, R} ->
			?INFO("Start File Logger: ~s~n", [?PERROR(R)])
	end,
	hermes_sup:start_link([]).

stop(_State) ->
	error_logger:logfile(close),
	ok.

