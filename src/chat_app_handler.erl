-module(chat_app_handler).
-behaviour(gen_server).
-include("chat_app.hrl").

-export([
  start_link/0
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

%% +--------------------------------------------------------------+
%% |                       Macros & Records                       |
%% +--------------------------------------------------------------+

-record(state, {
  clients
}).

%% +--------------------------------------------------------------+
%% |                        API functions                         |
%% +--------------------------------------------------------------+

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, #state{}, []).

%% +--------------------------------------------------------------+
%% |                    gen_server functions                      |
%% +--------------------------------------------------------------+

init(State) ->
  {ok, State#state{
    clients = ets:new(clients, [
      set,
      private,
      {read_concurrency, true},
      {write_concurrency, auto}
    ])
  }}.

handle_cast({error, Reason, Socket}, State) ->
  gen_tcp:close(Socket),
  ?LOGERROR("Socket error: ~p", [Reason]),
  {stop, Reason, State};

handle_cast({send, Sender, Message}, State = #state{clients = Clients}) ->
  case get_username(Clients, Sender) of
    none ->
      ignore;
    SenderUsername ->
      ?LOGINFO("[~p] ~s sent the message: ~s", [Sender, binary_to_list(SenderUsername), binary_to_list(Message)]),
      BroadcastMessage = build_message(SenderUsername, Message),
      broadcast_message(Clients, BroadcastMessage, Sender, SenderUsername)
  end,
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call({connect, Socket, Username}, _From, State = #state{clients = Clients}) ->
  case ets:lookup(Clients, Socket) of
    [] ->
      %% Insert the new client to the ETS table
      ets:insert(Clients, {Socket, Username}),
      %% Notify about connected client
      ?LOGINFO("[~p] ~s joined the chat.", [Socket, binary_to_list(Username)]),
      case gen_tcp:send(Socket, "Connected to the chat!") of
        ok ->
          ignore;
        {error, Reason} ->
          ?LOGERROR("Failed to send welcome message to ~p: ~p", [Socket, Reason])
      end,
      JoinMessage = <<Username/binary, (list_to_binary(" joined the chat."))/binary>>,
      broadcast_message(Clients, JoinMessage, Socket, Username),
      {reply, ok, State};
    [{_Key, Value}] when Value =:= Username ->
      {reply, {error, username_taken}, State}
  end;

handle_call({disconnect, Socket}, _From, State = #state{clients = Clients}) ->
  case ets:member(Clients, Socket) of
    true ->
      Username = get_username(Clients, Socket),
      case gen_tcp:send(Socket, "Disconnected from the chat!") of
        ok ->
          ignore;
        {error, Reason} ->
          ?LOGERROR("Failed to send welcome message to ~p: ~p", [Socket, Reason])
      end,
      %% Closing and removing socket
      gen_tcp:close(Socket),
      ets:delete(Clients, Socket),
      %% Notifying users about disconnected user
      DisconnectMessage = <<Username/binary, (list_to_binary(" left the chat."))/binary>>,
      broadcast_message(Clients, DisconnectMessage),
      ?LOGINFO("[~p] ~s left the chat.", [Socket, binary_to_list(Username)]),
      {reply, ok, State};
    false ->
      {reply, invalid_socket, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% +--------------------------------------------------------------+
%% |                     Internal functions                       |
%% +--------------------------------------------------------------+

build_message(Username, Message) ->
  <<Username/binary,
    (list_to_binary(" sent: "))/binary,
    Message/binary>>.

broadcast_message(Clients, Message) ->
  SendFunction =
    fun({Receiver, _Username}, _) ->
      gen_tcp:send(Receiver, Message)
    end,
  ets:foldl(SendFunction, none, Clients).

broadcast_message(Clients, Message, Sender, SenderUsername) ->
  SendFunction =
    fun({Receiver, Username}, _) ->
      case (Sender =/= Receiver) orelse (SenderUsername =/= Username) of
        true -> gen_tcp:send(Receiver, Message);
        false -> ok
      end
    end,
  ets:foldl(SendFunction, none, Clients).

get_username(Clients, Socket) ->
  case ets:lookup(Clients, Socket) of
    [] -> none;
    [{Socket, Username}] -> Username
  end.