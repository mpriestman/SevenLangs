%%% Protocol
%%% --------
%%%
%%% To server: {ClientPID, logon, UserName}
%%% Reply: {messenger, stop, user_exists_at_other_node} stops the client
%%% Reply: {messenger, logged_on} logon was successful
%%%
%%% To server: {ClientPID, logoff}
%%% Reply: {messenger, logged_off}
%%%
%%% To server: {ClientPID, logoff}
%%% Reply: no reply
%%%
%%% To server: {ClientPID, message_to, ToName, Message} sends a message
%%% Reply: {messenger, stop, you_are_not_logged_on} stops the client
%%% Reply: {messenger, receiver_not_found} no user with this name logged on
%%% Reply: {messenger, sent} message has been sent
%%%
%%% To client: {message_from, Name, Message}
%%%

-module(chat).
-export([start_server/0, server/0, server/1, logon/1, logoff/0, msg/2, client/2]).

server_node() ->
  messenger@iMac.
  
server() ->
  process_flag(trap_exit, true),
  server([]).  

%%% This is the server process for the "messenger"
%%% The user list has the format [{ClientPid1, Name1}, {ClientPid2, Name2}, ...]
server(User_List) ->
  receive
    {From, logon, Name} ->
      New_User_List = server_logon(From, Name, User_List),
      server(New_User_List);
    {'EXIT', From, _} ->
      New_User_List = server_logoff(From, User_List),
      server(New_User_List);
    {From, logoff} ->
      New_User_List = server_logoff(From, User_List),
      server(New_User_List);
    {From, message_to, To, Message} ->
      server_transfer(From, To, Message, User_List),
      io:format("list is now: ~p~n", [User_List]),
      server(User_List)
  end.

%%% Starts the server  
start_server() ->
  register(messenger, spawn(chat, server, [])).
  
%%% Adds new user to server logon list
server_logon(From, Name, User_List) ->
  case lists:keymember(Name, 2, User_List) of
    true ->
      From ! {messenger, stop, user_exists_at_other_node}, %reject logon
      User_List;
    false ->
      From ! {messenger, logged_on},
      link(From),
      [{From, Name} | User_List]
  end.

%%% Removes a user from server logon list  
server_logoff(From, User_List) ->
  lists:keydelete(From, 1, User_List).
  
%%% Sends a message to a user
server_transfer(From, To, Message, User_List) ->
  case lists:keysearch(From, 1, User_List) of
    false ->
      From ! {messenger, stop, you_are_not_logged_on};
    {value, {From, Name}} ->
      server_transfer(From, Name, To, Message, User_List)
  end.
  
%%% If the server exists, sends a message to the user
server_transfer(From, Name, To, Message, User_List) ->
  case lists:keysearch(To, 2, User_List) of
    false ->
      From ! {messenger, receiver_not_found};
    {value, {ToPid, To}} ->
      ToPid ! {message_from, Name, Message},
      From ! {messenger, sent}
  end.
  
%%% User commands
logon(Name) ->
  case whereis(mess_client) of
    undefined ->
      register(mess_client, spawn(chat, client, [server_node(), Name]));
      _ -> already_logged_on
  end.
  
logoff() ->
  mess_client ! logoff.
  
msg(ToName, Message) ->
  case whereis(mess_client) of
    undefined ->
      not_logged_on;
    _ ->
      mess_client ! {message_to, ToName, Message},
      ok
  end.
  
%%% The client process which runs on each server node
client(Server_Node, Name) ->
  {messenger, Server_Node} ! {self(), logon, Name},
  await_result(),
  client(Server_Node).
  
client(Server_Node) ->
  receive
    logoff ->
      {messenger, Server_Node} ! {self(), logoff},
      exit(normal);
    {message_to, ToName, Message} ->
      {messenger, Server_Node} ! {self(), message_to, ToName, Message},
      await_result();
    {message_from, FromName, Message} ->
      io:format("~p: ~p~n", [FromName, Message])
  end,
  client(Server_Node).
  
await_result() ->
  receive
    {messenger, stop, Why} ->
      io:format("~p~n", [Why]),
      exit(normal);
    {messenger, What} ->
      io:format("~p~n", [What])
  after 5000 ->
    io:format("No response from server~n"),
    exit(timeout)
  end.