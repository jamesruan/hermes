-define(PERROR(Msg), erl_posix_msg:message(Msg)).

-define(_FORMAT(Data), "[~p](~p:~p)~n" ++ Data ++ "~n", [?MODULE, ?FILE, ?LINE]).
-define(_FORMAT(Format, Data), "[~p](~p:~p)~n" ++ Format ++"~n", [?MODULE, ?FILE, ?LINE | Data]).
-define(_FORMATP(Data), "[~p](~p:~p)~n~p~n", [?MODULE, ?FILE, ?LINE, Data]).

-define(INFO(Data), error_logger:info_msg(?_FORMAT(Data))).
-define(INFO(Format, Data), error_logger:info_msg(?_FORMAT(Format,Data))).
-define(INFOP(Data), error_logger:info_msg(?_FORMATP(Data))).

-define(WARN(Data), error_logger:warning_msg(?_FORMAT(Data))).
-define(WARN(Format, Data), error_logger:warning_msg(?_FORMAT(Format,Data))).
-define(WARNP(Data), error_logger:warning_msg(?_FORMATP(Data))).

-define(ERROR(Data), error_logger:error_msg(?_FORMAT(Data))).
-define(ERROR(Format, Data), error_logger:error_msg(?_FORMAT(Format,Data))).
-define(ERRORP(Data), error_logger:error_msg(?_FORMATP(Data))).

