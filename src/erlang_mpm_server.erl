%%%-------------------------------------------------------------------
%%% @author  Rafał Trójniak <rafal@trojniak.net>
%%% @copyright (C) 2013 Rafał Trójniak. All Rights Reserved.
%%% @doc
%%%		Server for worker pool
%%% @end
%%% Created :  wto sty 29 20:57:12 2013 by Rafał Trójniak
%%%-------------------------------------------------------------------
%%% TODO Timeout internal handling
%%% TODO option scheduleAfterTimeout adding and implementation
%%% TODO While rescheduling, keep in mind to use another worker
%%% TODO Results refactor
-module(erlang_mpm_server).

-behaviour(gen_server).

%% API
%
-export([start_link/2, send_call/2, send_call/3, send_event/2]).

% Internal exports
-export([report/2, send_result/3, report_crash/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
		% Options
		startWorkers,
		minSpareWorkers,
		maxSpareWorkers,
		maxWorkers,
		maxTaskPerWorker,
		maxReschedule,
		queueMod,
		% Scoreboard
		startedWorkers=[],
		startingWorkers=[],
		readyWorkers=[],
		busyWorkers=[],
		stoppingWorkers=[],
		% State
		supervisor,
		queue
	}).

% Single job info
-record(job, {
		task,
		type,
		from=nil,
		callTime=nil,
		scheduled=0,
		timeout=nil
	}).

% Single worker info as added in scoreboard
-record(worker, {
		pid,
		jobCount=0,
		lastJob=nil,
		jobStarted=nil
	}).

send_call(Pid, Task)
	when is_pid(Pid) ;
		is_atom(Pid) ->
	send_call(Pid,Task,infinity).

send_call(Pid, Task,  Timeout )
	when
	is_pid(Pid) andalso is_integer(Timeout) ;
	is_pid(Pid) andalso Timeout == infinity ;
	is_atom(Pid) andalso is_integer(Timeout) ;
	is_atom(Pid) andalso Timeout == infinity ->
	%% TODO Calc TimeoutAt as current  + timeout
	Time=now(),
	gen_server:call(Pid, {new_call, Task, Time,Timeout}, Timeout).

send_event(Pid, Task )
	when is_pid(Pid);
		is_atom(Pid) ->
	gen_server:cast(Pid, {new_event, Task}).

%% Worker reports its state
report(Pid,starting)->
	gen_server:cast(Pid,{report,starting,self()});
report(Pid,ready)->
	gen_server:cast(Pid,{report,ready,self()});
report(Pid,finished)->
	gen_server:cast(Pid,{report,finished,self()}).

report_crash(Pid,Reason)->
	gen_server:cast(Pid,{report,crashed,self(), Reason}).

send_result(State, Pid,Result)->
	gen_server:cast(Pid,{result,self(),State, Result}).

%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
start_link(Supervisor, Options) ->
	case proplists:get_value(register,Options,undefined) of
		undefined ->
			gen_server:start_link(?MODULE, [Supervisor, Options], []);
		Name when is_atom(Name) ->
			gen_server:start_link({local,Name}, ?MODULE, [Supervisor, Options], [])
	end.

%%% gen_server callbacks

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
init([Supervisor, Options]) ->
	process_flag(trap_exit, true),
	QueueModule = proplists:get_value(queueModule, Options, queue),
	StartWorkers = proplists:get_value(startWorkers, Options, 1),
	MinSpareWorkers = proplists:get_value(minSpareWorkers, Options, 0),
	MaxSpareWorkers = proplists:get_value(maxSpareWorkers, Options, 1),
	MaxWorkers = proplists:get_value(maxWorkers, Options, 1),
	MaxReschedule = proplists:get_value(maxReschedule, Options, 0),
	MaxTaskPerWorker = proplists:get_value(maxTaskPerWorker, Options, 0),
	Queue=QueueModule:new(),
	gen_server:cast(self(),init),
	{ok, #state{
		% Options
		startWorkers=StartWorkers, maxWorkers=MaxWorkers,
		minSpareWorkers=MinSpareWorkers, maxSpareWorkers=MaxSpareWorkers,
		maxTaskPerWorker=MaxTaskPerWorker,
		maxReschedule=MaxReschedule,
		% Initial state
		supervisor=Supervisor,
		queue=Queue, queueMod=QueueModule
		}}.

%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
handle_call({new_call, Task, CallTime, Timeout}, From, State) ->
	QeueuMod=State#state.queueMod,
	NewJob=#job{task=Task, type=call, from=From, callTime=CallTime, timeout=Timeout},
	NewQueue=QeueuMod:in(NewJob,State#state.queue),
	ManagedState=manageWorkers(trySchedule(State#state{queue=NewQueue})),
	{noreply, ManagedState}.

%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%%
handle_cast({report,crashed,Pid, Reason}, State) ->
	case lists:keytake(Pid, #worker.pid, State#state.busyWorkers) of
		false ->
			StartedWorkers=
				lists:keydelete(Pid,#worker.pid, State#state.startedWorkers),
			StartingWorkers=
				lists:keydelete(Pid,#worker.pid, State#state.startingWorkers),
			ReadyWorkers=
				lists:keydelete(Pid,#worker.pid, State#state.readyWorkers),
			StoppingWorkers=
				lists:keydelete(Pid,#worker.pid, State#state.stoppingWorkers),
			NewState=State#state{
				startedWorkers=StartedWorkers,
				startingWorkers=StartingWorkers,
				readyWorkers=ReadyWorkers,
				stoppingWorkers=StoppingWorkers};
		{value, Worker, RemainingWorkers} ->
			LastJob=Worker#worker.lastJob,
			% Reschedule job
			if
				LastJob#job.scheduled<State#state.maxReschedule ->
					RescheduledJob=LastJob#job{scheduled=LastJob#job.scheduled+1},
					QeueuMod=State#state.queueMod,
					ModifiedQueue=QeueuMod:in_r(RescheduledJob,State#state.queue),
					NewState=trySchedule(State#state{queue=ModifiedQueue});
				true ->
					gen_server:reply(LastJob#job.from, {error,Reason}),
					NewState=State#state{busyWorkers=RemainingWorkers}
			end
	end,
	{noreply, manageWorkers(NewState)};
handle_cast({result, Pid, RetStatus, Result}, State) ->
	case lists:keytake(Pid, #worker.pid, State#state.busyWorkers) of
		false ->
			throw({pidNotInQueue,Pid});
		{value, Worker, NewList} ->
			% Send result
			LastJob=Worker#worker.lastJob,
			gen_server:reply(LastJob#job.from, {RetStatus ,Result}),
			handleFinishedJob(Worker, NewList, State)
	end;
handle_cast({report,finished,Pid}, State) ->
	case lists:keytake(Pid, #worker.pid, State#state.busyWorkers) of
		false ->
			throw({pidNotInQueue,Pid});
		{value, Worker, NewList} ->
			handleFinishedJob(Worker, NewList, State)
	end;
handle_cast({report,ready,Pid}, State) ->
	case lists:keytake(Pid, #worker.pid, State#state.startingWorkers) of
		false ->
			throw({pidNotInQueue,Pid});
		{value, Worker, NewList} ->
			NewState=State#state{startingWorkers=NewList,
				readyWorkers=[Worker| State#state.readyWorkers] },
			{noreply, manageWorkers(trySchedule(NewState))}
	end;
handle_cast({report,starting,Pid}, State) ->
	case lists:keytake(Pid, #worker.pid, State#state.startedWorkers) of
		false ->
			throw({pidNotInQueue,Pid});
		{value, Worker, NewList} ->
			{noreply, State#state{startedWorkers=NewList,
					startingWorkers=[Worker| State#state.startingWorkers]
				}
			}
	end;
handle_cast(init, State) ->
	StartedState=lists:foldl(
		fun(_,LastState) -> startWorker(LastState) end,
		State,
		lists:seq(1,State#state.startWorkers)),
	ManagedState=manageWorkers(StartedState),
	{noreply, ManagedState};
handle_cast({new_event, Task }, State) ->
	QeueuMod=State#state.queueMod,
	NewQueue=QeueuMod:in(#job{task=Task, type=event}, State#state.queue),
	ScheduledState=trySchedule(State#state{queue=NewQueue}),
	ManagedState=manageWorkers(ScheduledState),
	{noreply, ManagedState}.

%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
handle_info({'EXIT',_From, Reason}, State) ->
	{stop, Reason, State}.

%% @spec terminate(Reason, State) -> void()
terminate(_Reason, _State) ->
	ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Internal functions

% Tries to schedule job on workers
trySchedule(State) ->
	QueueMod=State#state.queueMod,
	case QueueMod:is_empty(State#state.queue) of
		true -> State;
		false ->
			case State#state.readyWorkers of
				[] ->
					State;
				[Worker| ReadyWorkers] ->
					{{value,Job},Queue}=QueueMod:out(State#state.queue),
					case Job#job.type of
						event ->
							erlang_mpm_worker:submit_event(Worker#worker.pid, Job#job.task);
						call ->
							erlang_mpm_worker:submit_call(Worker#worker.pid, Job#job.task)
					end,
					BusyWorker=Worker#worker{lastJob=Job, jobStarted=now()},
					trySchedule( State#state{queue=Queue, readyWorkers=ReadyWorkers,
							busyWorkers=[BusyWorker|State#state.busyWorkers] })
			end
	end.

% Manages worker count accourding to limits
manageWorkers(State) ->
	StartedReadyWorkers=
		length(State#state.startedWorkers) +
		length(State#state.startingWorkers) +
		length(State#state.readyWorkers),
	IdleWorkers=
		length(State#state.readyWorkers),
	if
		IdleWorkers > State#state.maxSpareWorkers ->
			manageWorkers( stopIdleWorker(State)) ;
		StartedReadyWorkers < State#state.minSpareWorkers ->
			AllWorkers=StartedReadyWorkers+ length(State#state.busyWorkers),
			if
				AllWorkers < State#state.maxWorkers ->
					manageWorkers( startWorker(State)) ;
				true ->
					State
			end;
		true -> State
	end.

stopIdleWorker(State) ->
	[Worker| ReadyWorkers]=State#state.readyWorkers,
	erlang_mpm_worker:stop(Worker#worker.pid),
	State#state{readyWorkers=ReadyWorkers,
		stoppingWorkers=[Worker| State#state.stoppingWorkers]}.

startWorker(State) ->
	Spec= supervisor:which_children(State#state.supervisor),
  {worker_sup,WorkerSup,_,_}=
		lists:keyfind(worker_sup, 1, Spec),
	{ok, Pid} = erlang_mpm_sup:start_worker(WorkerSup),
	NewWorker=#worker{pid=Pid},
	State#state{startedWorkers=[NewWorker|State#state.startedWorkers]}.

handleFinishedJob(Worker, NewList, State) ->
	CleanWorker=Worker#worker{
		jobCount=Worker#worker.jobCount+1,
		lastJob=nil},
	if
		State#state.maxTaskPerWorker ==0 ;
		State#state.maxTaskPerWorker > CleanWorker#worker.jobCount ->
			NewState=State#state{busyWorkers=NewList,
				readyWorkers=[CleanWorker| State#state.readyWorkers] },
			{noreply, manageWorkers(trySchedule(NewState))};
		true ->
			erlang_mpm_worker:stop(CleanWorker#worker.pid),
			NewState=State#state{busyWorkers=NewList,
				stoppingWorkers=[CleanWorker| State#state.stoppingWorkers] },
			{noreply, manageWorkers(trySchedule(NewState))}
	end.


%% vim: set ts=2 sw=2 ai invlist si cul nu:	
