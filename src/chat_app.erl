-module(chat_app).

-export([
  start_server/1,
  start_client/2,
  send_message/2,
  connect/2,
  disconnect/1
]).

%% Starts the chat server
start_server(Port) ->
  chat_app_server:start_link(?MODULE, Port, {chat_app_receiver, init}).

%% Connects to the chat server
start_client(Address, Port) ->
  {ok, IPv4} = inet:parse_address(Address),
  {ok, Socket} = gen_tcp:connect(IPv4, Port, [binary, {active, false}]),
  Socket.

connect(Socket, Username) ->
  gen_tcp:send(Socket, "connect:" ++ Username).

disconnect(Socket) ->
  gen_tcp:send(Socket, "leave").

send_message(Client, Message) ->
  gen_tcp:send(Client, "send:" ++ Message).