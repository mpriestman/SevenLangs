-module(my_bank).

-behaviour(gen_server).
-export([start/0, stop/0, new_account/1, deposit/2, withdraw/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

new_account(Who)      -> gen_server:call(?MODULE, {new, Who}).
deposit(Who, Amount)  -> gen_server:call(?MODULE, {add, Who, Amount}).
withdraw(Who, Amount) -> gen_server:call(?MODULE, {remove, Who, Amount}).

init([]) -> {ok, ets:new(?MODULE, [])}.

handle_call({new, Who}, _From, Tab) ->
  Reply = case ets:lookup(Tab, Who) of
            [] -> ets:insert(Tab, {Who, 0}),
                  {welcome, Who};
            [_] -> {Who, you_are_already_a_customer}
          end,
  {reply, Reply, Tab};

handle_call({add, Who, X}, _From, Tab) ->
  Reply = case ets:lookup(Tab, Who) of
            [] -> not_a_customer;
            [{Who, Balance}] ->
              NewBalance = Balance + X,
              ets:insert(Tab, {Who, NewBalance}),
              {thanks, Who, your_balance_is, NewBalance}
          end,
  {reply, Reply, Tab};

handle_call({remove, Who, X}, _From, Tab) ->
  Reply = case ets:lookup(Tab, Who) of
            [] -> not_a_customer;
            [{Who, Balance}] when X =< Balance ->
              NewBalance = Balance - X,
              ets:insert(Tab, {Who, NewBalance}),
              {thanks, Who, your_balance_is, NewBalance};
            [{Who, Balance}] ->
              {sorry, Who, you_only_have, Balance, in_the_bank}
          end,
  {reply, Reply, Tab};

handle_call(stop, _From, Tab) ->
  {stop, normal, stopped, Tab}.
  
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.