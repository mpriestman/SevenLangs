-module(user_interface).
-export([logon/1, logoff/0, message/2]).
-include("chat_interface.hrl").
-include("chat_config.hrl").

logon(Name) ->
  case whereis(mess_client) of
    undefined ->
      register(mess_client, spawn(chat_client, client, [?server_node, Name]));
    _ -> already_logged_on
  end.
  
logoff() ->
  mess_client ! logoff.
  
message(ToName, Message) ->
  case whereis(mess_client) of
    undefined ->
      not_logged_on;
    _ ->
      mess_client ! #message_to{to_name = ToName, message = Message},
      ok
  end.