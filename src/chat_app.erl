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
  Self = self(),
  {ok, Client} = chat_app_client:start_link(Self, Address, Port),
  #{pid => Self, client => Client}.

%% Join to the chat
connect(#{pid := PID, client := Client}, Username) ->
  gen_server:call(Client, {connect, PID, Username}).

%% Disconnect from the chat
disconnect(#{pid := PID, client := Client}) ->
  gen_server:call(Client, {disconnect, PID}).

%% Send message to the chat
send_message(#{pid := PID, client := Client}, Message) ->
  gen_server:call(Client, {send, PID, Message}).