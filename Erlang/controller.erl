-module(controller).
-import(lists, [reverse/1, reverse/2]).
-compile(export_all).

-include_lib("amqp_client/include/amqp_client.hrl").

add_task(JobId, [{JobId, Count} | T], L) -> [{JobId, Count + 1} | reverse(T, L)];
add_task(JobId, [H | T], L) -> add_task(JobId, T, [H | L]);
add_task(JobId, [], L) -> [{JobId, 1} | L].

remove_task(JobId, [{JobId, 1} | T], L) -> {done, JobId, reverse(T, L)};
remove_task(JobId, [{JobId, Count} | T], L) -> {running, JobId, [{JobId, Count - 1} | reverse(T, L)]};
remove_task(JobId, [H | T], L) -> remove_task(JobId, T, [H | L]);
remove_task(JobId, [], L) -> {no_job, JobId, reverse(L)}.

server() ->
  spawn(fun loop/0).
  
loop() ->
  loop([]).  

loop(Q) ->
  receive
    {start, JobId} ->
      Q1 = add_task(JobId, Q, []),
      io:format("Job List: ~p~n", [Q1]),
      loop(Q1);
    {finished, JobId} ->
      {Result, JobId, Q1} = remove_task(JobId, Q, []),
      io:format("Job List: ~p~n", [Q1]),
      loop(Q1)
  end.