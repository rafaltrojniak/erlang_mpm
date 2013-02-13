%%%-------------------------------------------------------------------
%%% @author  Rafał Trójniak <rafal@trojniak.net>
%%% @copyright (C) 2013 Rafał Trójniak. All Rights Reserved.
%%% @doc
%%%		Implementation of simple queue worker
%%% @end
%%% Created :  sob lut 09 17:18:33 2013 by Rafał Trójniak
%%%-------------------------------------------------------------------
-module(erlang_mpm_testapp).

%% API
-export([send_event/1, send_call/1, start_sup/0]).
%% CallBacks
-export([init/1, process_event/2, process_call/2, terminate/1]).

-define(SERVER,?MODULE).

%%% API

% Supervisor initiation - wrapper for erlang_mpm_sup:start_link with options set
start_sup() ->
	Options=[
		{startWorkers, 3},
		{minSpareWorkers, 3},
		{maxSpareWorkers, 5},
		{maxWorkers, 30},
		{maxReschedule, 2},
		{maxTaskPerWorker, 10},
		{register,?SERVER}
	],
	erlang_mpm_sup:start_link(?MODULE,Options).

send_call(Message) ->
	erlang_mpm_server:send_call(?SERVER,Message).
send_event(Message) ->
	erlang_mpm_server:send_event(?SERVER,Message).

%% CallBacks

% Worker initiation function - prepares worker for functionning:
% - Pre-compute pre-cache values
% - Test/Make connections to needed resources
init(_Options)->
	io:format("~p starting initiation\n",[self()]),
	timer:sleep(1000),
	io:format("~p finishing initiation\n",[self()]),
	State=[],
	{ok,State}.

% Cleanups after worker shutdown
terminate(_State)->
	io:format("~p starting termination\n",[self()]),
	timer:sleep(1000),
	io:format("~p finishing termination\n",[self()]),
	ok.

% Processess single evnt
process_event(_State, Job) when Job == arith->
	io:format("~p starting event ~p\n",[self(),Job]),
	X=0,
	Ret=10/X,
	io:format("~p finishing event ~p\n",[self(),Job]),
	{error,Ret};
process_event(_State, Job) when Job == undef->
	io:format("~p starting event ~p\n",[self(),Job]),
	undefModule:throw(fail),
	io:format("~p finishing event ~p\n",[self(),Job]);
process_event(_State, Job) when Job == throw->
	io:format("~p starting event ~p\n",[self(),Job]),
	throw(fail),
	io:format("~p finishing event ~p\n",[self(),Job]);
process_event(State, Job) when Job == error->
	io:format("~p starting event ~p\n",[self(),Job]),
	timer:sleep(1000),
	io:format("~p finishing event ~p\n",[self(),Job]),
	{error,State};
process_event(State, Job)->
	io:format("~p starting event ~p\n",[self(),Job]),
	timer:sleep(1000),
	io:format("~p finishing event ~p\n",[self(),Job]),
	{ok,State}.

% Process single call
process_call(_State, Job) when Job == arith->
	io:format("~p starting event ~p\n",[self(),Job]),
	X=0,
	Ret=10/X,
	io:format("~p finishing event ~p\n",[self(),Job]),
	{error,Ret};
process_call(_State, Job) when Job == undef->
	io:format("~p starting event ~p\n",[self(),Job]),
	undefModule:throw(fail),
	io:format("~p finishing event ~p\n",[self(),Job]);
process_call(_State, Job) when Job == throw->
	io:format("~p starting event ~p\n",[self(),Job]),
	throw(fail),
	io:format("~p finishing event ~p\n",[self(),Job]);
process_call(State, Job) when Job == error ->
	io:format("~p starting call ~p\n",[self(),Job]),
	timer:sleep(1000),
	io:format("~p finishing call ~p\n",[self(),Job]),
	{error,[],State};
process_call(State, Job)->
	io:format("~p starting call ~p\n",[self(),Job]),
	timer:sleep(1000),
	io:format("~p finishing call ~p\n",[self(),Job]),
	{ok,[],State}.

%%% Internal functions



%% vim: set ts=2 sw=2 ai invlist si cul nu:
