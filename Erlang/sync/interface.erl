-module(interface).
-compile(export_all).

init() ->
	server:list().

list_client(Pid) ->
	{ok, Data} = client:list(Pid),
	Data.

list_server(_State) ->
	server:list().

server_incremental() ->
	false.

get_server_actions(Base, State) ->
	[].

compare(V1, V2) ->
	{S1, _} = V1,
	{S2, _} = V2,
	string:equal(S1, S2).

compare_all(V1, V2) ->
	V1 == V2.

resolve(A) ->
	resolve_interactive(A).

resolve_theirs({Key, Action1, _Action2, Value1, _Value2}) ->
	{Key, Action1, Value1}.

resolve_yours({Key, _Action1, Action2, _Value1, Value2}) ->
	{Key, Action2, Value2}.

resolve_interactive({Key, Action1, Action2, Value1, Value2}) ->
	io:format("Conflict!~n"),
	io:format("Theirs: ~p <~p>~n", [Value1, Action1]),
	io:format("Yours: ~p <~p>~n", [Value2, Action2]),
	case io:read("Select (yours/theirs): ") of
		{ok, yours} ->
			{Key, Action2, Value2};
		{ok, theirs} ->
			{Key, Action1, Value1}
	end.

apply_server({Key, added, {Value, _Change}}, _State) ->
	io:format("Server Adding: ~p=~p~n", [Key, Value]),
	{ok, Updated} = server:add(Key, Value),
	{added, Updated};
apply_server({Key, differ, {Value, _Change}}, _State) ->
	io:format("Server Updating: ~p=~p~n", [Key, Value]),
	{ok, Updated} = server:update(Key, Value),
	{updated, Updated};
apply_server({Key, deleted, {_Value, _Change}}, _State) ->
	io:format("Server Deleting: ~p~n", [Key]),
	{ok, Key} = server:delete(Key),
	{deleted, Key}.
	
apply_client({Key, added, Value}, Pid) ->
	io:format("Client Adding: ~p=~p~n", [Key, Value]),
	client:set(Pid, Key, Value);
apply_client({Key, differ, Value}, Pid) ->
	io:format("Client Updating: ~p=~p~n", [Key, Value]),
	client:set(Pid, Key, Value);
apply_client({Key, delete, _Value}, Pid) ->
	io:format("Client Deleting: ~p~n", [Key]),
	client:delete(Pid, Key).
