#!/usr/bin/env escript
%%! -pz ./amqp_client ./rabbit_common ./amqp_client/ebin ./rabbit_common/ebin

-include_lib("amqp_client/include/amqp_client.hrl").

main(Argv) ->
  {ok, Connection} =
    amqp_connection:start(#amqp_params_network{host = "localhost"}),
  {ok, Channel} =
    amqp_connection:open_channel(Connection),
    
  amqp_channel:call(Channel, #'queue.declare'{queue = <<"hello">>}),
  
  Message = case Argv of
    [] -> <<"Hello World">>;
    Msg -> list_to_binary(string:join(Msg, " "))
  end,
  
  amqp_channel:cast(Channel,
    #'basic.publish'{
      exchange = <<"">>,
      routing_key = <<"hello">>},
    #amqp_msg{payload = Message}),
  
  io:format(" [x] Sent 'Hello World!~n"),
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok.