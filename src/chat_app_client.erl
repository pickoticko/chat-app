-module(chat_app_client).
-behaviour(gen_server).
-include("chat_app.hrl").

-export([
  start_link/3
]).

%% +--------------------------------------------------------------+
%% |                     gen_server callbacks                     |
%% +--------------------------------------------------------------+

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(DEFAULT_TCP_OPTIONS, [
  binary,
  {packet, 0},
  {active, false},
  {reuseaddr, true}
]).

-record(client, {
  socket = undefined,
  pid = undefined,
  receiver = undefined
}).

start_link(PID, Address, Port) ->
  logger:set_primary_config(level, info),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [PID, Address, Port], []).

init([PID, Address, Port]) ->
  {ok, IPv4} = inet:parse_address(Address),
  case gen_tcp:connect(IPv4, Port, [binary, {active, false}]) of
    {ok, Socket} ->
      Owner = self(),
      ReceiverPID = spawn(fun() -> message_receive(Owner, Socket) end),
      {ok, #client{
        receiver = ReceiverPID,
        socket = Socket,
        pid = PID
      }};
    {error, Reason} ->
      {stop, Reason}
  end.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call({connect, PID, Username}, _From, State = #client{pid = PID}) ->
  send_message("connect:" ++ Username, State);

handle_call({disconnect, PID}, _From, State = #client{pid = PID}) ->
  send_message("disconnect", State);

handle_call({send, PID, Message}, _From, State = #client{pid = PID}) ->
  case length(Message) > 0 of
    true -> send_message("send:" ++ string:strip(Message), State);
    false -> {reply, empty_message, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, invalid_command, State}.

handle_info({message, PID, Packet}, State = #client{receiver = PID}) ->
  ?LOGINFO("~s", [binary_to_list(Packet)]),
  {noreply, State};

handle_info({error, PID, Reason}, State = #client{receiver = PID, socket = Socket}) ->
  ?LOGINFO("Socket [~p] error: ~p", [Socket, Reason]),
  {stop, Reason, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State = #client{socket = Socket}) ->
  gen_tcp:close(Socket),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

send_message(Message, State = #client{socket = Socket}) ->
  case gen_tcp:send(Socket, Message) of
    ok -> {reply, ok, State};
    {error, Reason} -> {stop, Reason, State}
  end.

message_receive(Owner, Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Packet} ->
      Owner ! {message, self(), Packet},
      message_receive(Owner, Socket);
    {error, Reason} ->
      Owner ! {error, self(), Reason}
  end.
