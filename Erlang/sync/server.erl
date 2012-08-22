%%%-------------------------------------------------------------------
%%% File    : gen_server_template.full
%%% Author  : my name <yourname@localhost.localdomain>
%%% Description : 
%%%
%%% Created :  2 Mar 2007 by my name <yourname@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(server).

-behaviour(gen_server).
-define(SERVER, server).

%% API
-export([start/0, stop/0, add/2, update/2, delete/1, list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {data = [], change_id = 1000}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

add(Id, Value) -> gen_server:call(?MODULE, {add, Id, Value}).
update(Id, NewValue) -> gen_server:call(?MODULE, {update, Id, NewValue}).
delete(Id) -> gen_server:call(?MODULE, {delete, Id}).
list() -> gen_server:call(?MODULE, {list}).

%%====================================================================
%% Internal functions
%%====================================================================

data_update(Id, NewValue, ChangeId, Data) ->
  lists:keystore(Id, 1, Data, {Id, {NewValue, ChangeId}}).
  
data_delete(Id, Data) ->
  lists:keydelete(Id, 1, Data).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add, Id, Value}, _From, State) ->
    #state{data = Data, change_id = ChangeId} = State,
    State1 = State#state{data = data_update(Id, Value, ChangeId, Data), change_id = ChangeId + 1},
    Reply = ok,
    {reply, Reply, State1};
    
handle_call({update, Id, NewValue}, _From, State) ->
  #state{data = Data, change_id = ChangeId} = State,
  State1 = State#state{data = data_update(Id, NewValue, ChangeId, Data), change_id = ChangeId + 1},
  Reply = ok,
  {reply, Reply, State1};

handle_call({delete, Id}, _From, State) ->
  #state{data = Data, change_id = ChangeId} = State,
  State1 = State#state{data = data_delete(Id, Data), change_id = ChangeId + 1},
  Reply = ok,
  {reply, Reply, State1};

handle_call({list}, _From, State) ->
    #state{data = Reply} = State,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
