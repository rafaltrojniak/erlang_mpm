-module(erlang_mpm_app).

-behaviour(application).

%% Application callbacks
-export([start/2, start/0, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%%
start() ->
	application:start(erlang_mpm).

start(_StartType, _StartArgs) ->
	Options = [
	],
	case erlang_mpm_testapp:start_sup() of
	{ok, Pid} ->
		{ok, Pid};
	Error ->
		Error
		end.

stop(_State) ->
    ok.
