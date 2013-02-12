%%%-------------------------------------------------------------------
%%% @author  Rafał Trójniak <rafal@trojniak.net>
%%% @copyright (C) 2013 Rafał Trójniak. All Rights Reserved.
%%% @doc
%%%		Server for single worker process
%%% @end
%%% Created :  wto sty 29 20:57:32 2013 by Rafał Trójniak
%%%-------------------------------------------------------------------
-module(erlang_mpm_worker).

-behaviour(gen_fsm).

%% API
-export([start_link/3, submit_call/2, submit_event/2, stop/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
%% states
-export([starting/2, ready/2]).

-record(state, {
		localModule,
		workerState=[],
		workerOptions,
		master
	}).

%%% API

submit_call(Pid, Job) when is_pid(Pid) ->
	gen_fsm:send_event(Pid, {job_call, Job}).
submit_event(Pid, Job) when is_pid(Pid) ->
	gen_fsm:send_event(Pid, {job_event, Job}).

stop(Pid) when is_pid(Pid) ->
	gen_fsm:send_event(Pid, stop).

%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
start_link(LocalModule,Options, Master)
		when is_atom(LocalModule) andalso is_list(Options) andalso is_pid(Master)->
	gen_fsm:start_link(?MODULE, [LocalModule,Options, Master ], []).

%%% gen_fsm callbacks

%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
init([LocalModule,Options, Master]) ->
	process_flag(trap_exit, true),
	WorekerOptions=proplists:get_value(workerOptions,Options,[]),
	State=#state{master=Master, localModule=LocalModule,
			workerOptions=WorekerOptions},
	{ok, starting, State, 0}.

%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
starting(timeout, State) ->
	erlang_mpm_server:report(State#state.master,starting),
	Module=State#state.localModule,
	try
		{ok,WorkerState}=Module:init(State#state.workerOptions),
		erlang_mpm_server:report(State#state.master,ready),
		{next_state, ready, State#state{workerState=WorkerState}}
	catch
		throw:Term -> {stop,{throw,Term},State};
		exit:Reason -> {stop,{exit,Reason},State};
		error:Reason -> {stop,{error,{Reason,erlang:get_stacktrace()}},State}
	end.

ready(stop, State) ->
	Module=State#state.localModule,
	try
		case Module:terminate(State#state.workerState) of
			ok ->
				{stop, normal , State#state{workerState=nil}}
				%TODO Other return codes
		end
	catch
		throw:Term -> {stop,{throw,Term},State};
		exit:Reason -> {stop,{exit,Reason},State};
		error:Reason -> {stop,{error,{Reason,erlang:get_stacktrace()}},State}
	end;
ready({job_call, Job}, State) ->
	Module=State#state.localModule,
	try
		case Module:process_call(State#state.workerState,Job) of
			{ok,Result,NewWorkerState} ->
				erlang_mpm_server:send_result(ok, State#state.master,Result),
				{next_state, ready, State#state{workerState=NewWorkerState}};
			{error,Result,NewWorkerState} ->
				erlang_mpm_server:send_result(error, State#state.master,Result),
				{next_state, ready, State#state{workerState=NewWorkerState}};
			Other ->
				{stop,{badRet,Other},State}
		end
	catch
		throw:Term -> {stop,{throw,Term},State};
		exit:Reason -> {stop,{exit,Reason},State};
		error:Reason -> {stop,{error,{Reason,erlang:get_stacktrace()}},State}
	end;
ready({job_event, Job}, State) ->
	Module=State#state.localModule,
	try
		case Module:process_event(State#state.workerState,Job) of
			{ok,NewWorkerState} ->
				erlang_mpm_server:report(State#state.master,finished),
				{next_state, ready, State#state{workerState=NewWorkerState}};
			Other ->
				{stop,{badRet,Other},State}
		end
	catch
		throw:Term -> {stop,{throw,Term},State};
		exit:Reason -> {stop,{exit,Reason},State};
		error:Reason -> {stop,{error,{Reason,erlang:get_stacktrace()}},State}
	end.


%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%state_name(_Event, _From, State) ->
%	Reply = ok,
%	{reply, Reply, state_name, State}.

%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
handle_sync_event(_Event, _From, StateName, State) ->
	Reply = ok,
	{reply, Reply, StateName, State}.

%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
handle_info({'EXIT',_From, Reason}, _StateName, State) ->
	{stop, Reason, State}.

%% @spec terminate(Reason, StateName, State) -> void()
terminate(Reason, _StateName, State) ->
		erlang_mpm_server:report_crash(State#state.master, Reason),
	ok.

%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

%%% Internal functions



%% vim: set ts=2 sw=2 ai invlist si cul nu:
