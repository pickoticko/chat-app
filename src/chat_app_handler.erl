-module(chat_app_handler).
-behaviour(gen_server).

-include("chat_app.hrl").

-export([
  start_link/0
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {
  clients
}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, #state{}, []).

init(State) ->
  {ok, State#state{
    clients = ets:new(clients, [
      set,
      private,
      {read_concurrency, true},
      {write_concurrency, auto}
    ])
  }}.

handle_cast({send, Sender, Message}, State = #state{clients = Clients}) ->
  case get_username(Clients, Sender) of
    none ->
      ignore;
    SenderUsername ->
      ?LOGINFO("[~p] ~p sent the message: ~p", [Sender, binary_to_list(SenderUsername), binary_to_list(Message)]),
      SendFunction =
        fun({Receiver, Username}, _) ->
          case (Sender =/= Receiver) orelse (SenderUsername =/= Username) of
            true -> gen_tcp:send(Receiver, Message);
            false -> ok
          end
        end,
      ets:foldl(SendFunction, none, Clients)
  end,
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call({connect, Socket, Username}, _From, State = #state{clients = Clients}) ->
  case ets:lookup(Clients, Socket) of
    [] ->
      ?LOGINFO("[~p] ~p joined the chat.", [Socket, binary_to_list(Username)]),
      ets:insert(Clients, {Socket, Username}),
      {reply, ok, State};
    [{_Key, Value}] when Value =:= Username ->
      {reply, {error, username_taken}, State}
  end;

handle_call({disconnect, Socket}, _From, State = #state{clients = Clients}) ->
  case ets:member(Clients, Socket) of
    true ->
      Username = get_username(Clients, Socket),
      gen_tcp:close(Socket),
      ets:delete(Clients, Socket),
      ?LOGINFO("[~p] ~p left the chat.", [Socket, binary_to_list(Username)]),
      {reply, ok, State};
    false ->
      {reply, already_disconnected, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

get_username(Clients, Socket) ->
  case ets:lookup(Clients, Socket) of
    [] -> none;
    [{Socket, Username}] -> Username
  end.