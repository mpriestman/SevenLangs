-record(logon, {client_pid, username}).
-record(message, {client_pid, to_name, message}).

-record(abort_client, {message}).
-record(server_reply, {message}).
-record(message_from, {from_name, message}).

-record(message_to, {to_name, message}).