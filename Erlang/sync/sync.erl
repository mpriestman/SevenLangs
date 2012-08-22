-module(sync).
-compile(export_all).

make_base() ->
	[{1, {"abcde", 3122}},
	 {2, {"671df", 6909}}, 
	 {3, {"a6612", 6909}}, 
	 {4, {"129aa", 6909}},
	 {5, {"fee19", 6909}}].

make_server() ->
	[{1, {"abcde", 3122}}, 
	 {2, {"9914a", 7934}},
	 {4, {"129aa", 6909}}, 
	 {6, {"6693c", 7934}},
	 {7, {"6c997", 8800}}]. 

make_client() ->
	[{1, {"abcde", 3122}},
	 {2, {"11aa7", 0}},
	 {3, {"a66ff", 0}},
	 {7, {"6c997", 0}},
	 {8, {"fee19", 0}}].

test(ResolveFn) ->
	Base = make_base(),
	Server = make_server(),
	Client = make_client(),
	DeltaC = compare(Base, Client, fun compare_string/2),
	DeltaS = compare(Base, Server, fun compare_string/2),
	Actions = merge(DeltaS, DeltaC, ResolveFn),
	apply_actions(Actions, Base, fun update_change/1).

pass([{Key, _}|T], Result, Fn) ->
	Result1 = case dict:is_key(Key, Result) of
				  true -> Result;
				  false -> dict:store(Key, 0, Result)
			  end,
	Result2 = dict:store(Key, Fn(dict:fetch(Key, Result1)), Result1),
	pass(T, Result2, Fn);
pass([], Result, _Fn) ->
	Result.

pass1(List, Result) ->
	pass(List, Result, fun(A) -> A + 1 end).

pass2(List, Result) ->
	pass(List, Result, fun(A) -> A - 1 end).

compare(L1, L2, Fn) ->
	Res = dict:to_list(pass2(L2, pass1(L1, dict:new()))),
	Res1 = lists:map(fun(A) -> to_action(A, L1, L2, Fn) end, Res),
	lists:sort(lists:filter(fun not_same/1, Res1)).

to_action({Key, -1}, _L1, L2, _Fn) ->
	{value, {_, V}} = lists:keysearch(Key, 1, L2),
	{Key, added, V};
to_action({Key, 0}, L1, L2, Fn) ->
	compare_value(Key, L1, L2, Fn);
to_action({Key, 1}, L1, _L2, _Fn) ->
	{value, {_, V}} = lists:keysearch(Key, 1, L1),
	{Key, deleted, V}.

not_same({_, same}) ->
	false;
not_same(_) ->
	true.

compare_value(Key, L1, L2, Fn) ->
	{value, {K, V1}} = lists:keysearch(Key, 1, L1),
	{value, {K, V2}} = lists:keysearch(Key, 1, L2),
	case Fn(V1, V2) of
		true ->
			{Key, same};
		false ->
			{Key, differ, V2}
	end.

compare_user(V1, V2) ->
	string:equal(V1, V2).

compare_string(V1, V2) ->
	{S1, _} = V1,
	{S2, _} = V2,
	string:equal(S1, S2).

find_action(Key, [{Key, Action, _Value}|_T]) ->
	Action;
find_action(Key, [_H|T]) ->
	find_action(Key, T);
find_action(_, []) ->
	error.

find_value(Key, [{Key, _Action, Value}|_T]) ->
	Value;
find_value(Key, [_H|T]) ->
	find_value(Key, T);
find_value(_, []) ->
	error.

not_conflict({_, deleted, deleted, _, _}) ->
	false;
not_conflict(_) ->
	true.

resolve({Key, deleted, deleted, Value, _}, _Fn) ->
	{Key, deleted, Value};
resolve(A, Fn) ->
	Fn(A).

resolve_theirs({Key, Action1, _Action2, Value1, _Value2}) ->
	{Key, Action1, Value1}.

resolve_yours({Key, _Action1, Action2, _Value1, Value2}) ->
	{Key, Action2, Value2}.

merge(Delta1, Delta2, ResolveFn) ->
	Int = lists:map(fun({Key, _, _}) -> Key end, lists:filter(fun({Key, _, _}) -> lists:keymember(Key, 1, Delta2) end, Delta1)),
	Conflicts = lists:map(fun(Key) -> {Key, find_action(Key, Delta1), find_action(Key, Delta2), find_value(Key, Delta1), find_value(Key, Delta2)} end, Int),
	Resolved = lists:map(fun(A) -> resolve(A, ResolveFn) end, Conflicts),
	Delta1Only = lists:filter(fun({Key, _, _}) -> lists:keymember(Key, 1, Conflicts) == false end, Delta1),
	Delta2Only = lists:filter(fun({Key, _, _}) -> lists:keymember(Key, 1, Conflicts) == false end, Delta2),
	lists:merge3(Resolved, Delta1Only, Delta2Only).

update_none(Value) ->
	 Value.

update_change({Data, 0}) ->
	{Data, 9122};
update_change(Value) ->
	Value.

apply_actions(Actions, List, UpdateFn) ->
	lists:map(fun({Key, Value}) -> {Key, UpdateFn(Value)} end, apply1(Actions, List)).

apply1([{Key, added, Value}|T], List) ->
	apply1(T, [{Key, Value}|List]);
apply1([{Key, deleted, _Value}|T], List) ->
	apply1(T, lists:keydelete(Key, 1, List));
apply1([{Key, differ, Value}|T], List) ->
	apply1(T, lists:keyreplace(Key, 1, List, {Key, Value}));
apply1([], List) ->
	lists:sort(List).
	
