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
-export([call_sync/1, call_async/1, start_sup/0]).
%% CallBacks
-export([init/1, process_event/2, process_call/2, terminate/1]).

-define(SERVER,?MODULE).

%%% API

start_sup() ->
	Options=[
		{startWorkers, 3},
		{minSpareWorkers, 3},
		{maxSpareWorkers, 5},
		{maxWorkers, 30},
		{maxTaskPerWorker, 10},
		{register,?SERVER}
	],
	erlang_mpm_sup:start_link(?MODULE,Options).

call_sync(Message) ->
	erlang_mpm_server:call(?SERVER,Message).
call_async(Message) ->
	erlang_mpm_server:submit(?SERVER,Message).

%% CallBacks
init(_Options)->
	io:format("~p starting initiation\n",[self()]),
	timer:sleep(1000),
	io:format("~p finishing initiation\n",[self()]),
	State=[],
	{ok,State}.
terminate(_State)->
	io:format("~p starting termination\n",[self()]),
	timer:sleep(1000),
	io:format("~p finishing termination\n",[self()]),
	ok.
process_event(State, Job)->
	io:format("~p starting event ~p\n",[self(),Job]),
	timer:sleep(1000),
	io:format("~p finishing event ~p\n",[self(),Job]),
	{ok,State}.

process_call(State, Job)->
	io:format("~p starting call ~p\n",[self(),Job]),
	timer:sleep(1000),
	io:format("~p finishing call ~p\n",[self(),Job]),
	{ok,[],State}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%% Internal functions



%% vim: set ts=2 sw=2 ai invlist si cul nu:
