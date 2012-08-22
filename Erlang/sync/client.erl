-module(client).
-compile(export_all).

start() ->
  spawn(fun() -> client(server:list()) end).
  
stop(Pid) ->
  rpc(Pid, {stop}).

add(Pid, Id, Value) ->
  update(Pid, Id, Value).

update(Pid, Id, Value) ->
  rpc(Pid, {update, Id, Value}).
  
delete(Pid, Id) ->
  rpc(Pid, {delete, Id}).
  
list(Pid) ->
  rpc(Pid, {list}).
  
rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} -> Response
  end.
  
client(Data) ->
  loop(Data, Data).
  
loop(Data, Base) ->
  receive
    {From, {update, Id, NewValue}} ->
      Data1 = data_update(Id, NewValue, Data),
      From ! {self(), ok},
      loop(Data1, Base);
    {From, {delete, Id}} ->
      Data1 = lists:keydelete(Id, 1, Data),
      From ! {self(), ok},
      loop(Data1, Base);
    {From, {list}} ->
      From ! {self(), {ok, Data}},
      loop(Data, Base);
    {From, {sync}} ->
      case data_sync(Data, Base) of
        {ok, Data1} ->
          From ! {self(), ok},
          loop(Data1, Data1);
        {error} ->
          From ! {self(), fail},
          loop(Data, Base)
      end;
    {From, {stop}} ->
      From ! {self(), ok}
  end.
      
data_update(Id, NewValue, Data) ->
  lists:keystore(Id, 1, Data, {Id, {NewValue, 0}}).
  
data_sync(Data, Base) -> void.
  