%%%-------------------------------------------------------------------
%%% @author  Rafał Trójniak <rafal@trojniak.net>
%%% @copyright (C) 2013 Rafał Trójniak. All Rights Reserved.
%%% @doc
%%%		Supervisor for main process
%%% @end
%%% Created :  wto sty 29 20:52:42 2013 by Rafał Trójniak
%%%-------------------------------------------------------------------
-module(erlang_mpm_sup).

-behaviour(supervisor).

%% API
-export([start_link/2, start_worker/1]).

%% Supervisor callbacks
-export([init/1]).


%%% API functions
start_worker(Supervisor)
    when is_pid(Supervisor)->
	supervisor:start_child(Supervisor, [self()]).

%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
start_link(LocalModule,Options) ->
	supervisor:start_link(?MODULE, [LocalModule,Options]).

%%% Supervisor callbacks

%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
init([LocalModule,Options]) ->
	RestartStrategy = one_for_all,
	MaxRestarts = 10,
	MaxSecondsBetweenRestarts = 60,

	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

	Restart = permanent,
	Shutdown = 2000,

	Server = {
		server,
		{erlang_mpm_server, start_link, [self(),Options]},
		Restart, Shutdown, worker, [erlang_mpm_server]
	},
	ChildSup = {
		worker_sup,
		{erlang_mpm_worker_sup, start_link, [LocalModule,Options]},
		Restart, Shutdown, supervisor, [erlang_mpm_worker_sup]
	},

	{ok, {SupFlags, [Server , ChildSup ]}}.


%%% Internal functions



%% vim: set ts=2 sw=2 ai invlist si cul nu:	
