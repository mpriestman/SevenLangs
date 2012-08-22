-module(chat_client).
-export(client/2).
-include("chat_interface.hrl").

client(Server_Node, Name) ->
  {messenger, Server_Node} ! #logon{client_pid = self(), username = Name},
  await_result(),
  client(Server_Node).
  
client(Server_Node) ->
  receive
    logoff ->
      exit(normal);
    #message_to{to_name = ToName, message = Message} ->
      {messenger, Server_Node} ! #message{client_pid = self(), to_name = ToName, message = Message},
      await_result();
    {message_from, FromName, Message} ->
      io:format("~p: ~p~n", [FromName, Message])
  end,
  client(Server_Node).
  
await_result() ->
  receive
    #abort_client{message = Why} ->
      io:format("~p~n", [Why]),
      exit(normal);
    #server_reply{message = What} ->
      io:format("~p~n", [Why])
  after 5000 ->
    io:format("No response from server~n"),
    exit(timeout)
  end.