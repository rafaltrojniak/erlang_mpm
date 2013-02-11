%%%-------------------------------------------------------------------
%%% @author  Rafał Trójniak <rafal@trojniak.net>
%%% @copyright (C) 2013 Rafał Trójniak. All Rights Reserved.
%%% @doc
%%%		Supervisor for worker processes
%%% @end
%%% Created :  wto sty 29 20:52:20 2013 by Rafał Trójniak
%%%-------------------------------------------------------------------
-module(erlang_mpm_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/2, start_worker/2]).

%% Supervisor callbacks
-export([init/1]).

%%% API functions

%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
start_link(LocalModule,Options) ->
	supervisor:start_link(?MODULE, [LocalModule,Options]).

%%% Supervisor callbacks
start_worker(Pid, Id)
	 when is_pid(Pid)	->
  supervisor:start_child(Pid, [self(), Id]).

%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
init([LocalModule,Options]) ->
	RestartStrategy = simple_one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,

	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

	Restart = temporary,
	Shutdown = 2000,
	Type = worker,

	Child = {worker,
		{erlang_mpm_worker, start_link, [LocalModule,Options]},
		Restart, Shutdown, Type, [erlang_mpm_worker]},

	{ok, {SupFlags, [Child ]}}.

%%% Internal functions


%% vim: set ts=2 sw=2 ai invlist si cul nu:	
