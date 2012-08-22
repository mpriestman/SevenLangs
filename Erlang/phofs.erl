-module(phofs).
-export([mapreduce/4]).

-import(lists, [foreach/2]).

mapreduce(F1, F2, Acc0, L) ->
  S = self(),
  Pid = spawn(fun() -> reduce(S, F1, F2, Acc0, L) end),
  receive
    {Pid, Result} ->
      Result
  end.
  
reduce(Parent, F1, F2, Acc0, L) ->
  process_flag(trap_exit, true),
  ReducePid = self(),
  foreach(fun(X) ->
            spawn_link(fun() -> do_job(ReducePid, F1, X) end)
          end, L),
  N = length(L),
  Dict0 = dict:new(),
  Dict1 = collect_replies(N, Dict0),
  Acc = dict:fold(F2, Acc0, Dict1),
  Parent ! {self(), Acc}.

collect_replies(0, Dict) ->
  Dict;
collect_replies(N, Dict) ->
  receive
    {Key, Val} ->
      case dict:is_key(Key, Dict) of
        true ->
          Dict1 = dict:append(Key, Val, Dict),
          collect_replies(N, Dict1);
        false ->
          Dict1 = dict:store(Key, [Val], Dict),
          collect_replies(N, Dict1)
      end;
    {'EXIT', _, _Why} ->
      collect_replies(N - 1, Dict)
  end.
  
do_job(ReducePid, F, X) ->
  F(ReducePid, X).