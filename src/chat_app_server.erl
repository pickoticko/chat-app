-module(chat_app_server).
-behaviour(gen_server).

-export([
  start_link/3
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

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(state, {
  clients,
  port,
  loop,
  ip = any,
  listen_socket = null
}).

start_link(ServerName, Port, LoopFun) ->
  State = #state{
    port = Port,
    loop = LoopFun,
    clients = ets:new(clients, [
      set,
      public,
      {read_concurrency, true},
      {write_concurrency, auto}
    ])
  },
  gen_server:start_link({local, ServerName}, ?MODULE, State, []).

init(State = #state{
  port = Port
}) ->
  case gen_tcp:listen(Port, ?TCP_OPTIONS) of
    {ok, ListenSocket} ->
      {ok, accept(State#state{listen_socket = ListenSocket})};
    {error, Reason} ->
      {stop, Reason}
  end.

accept(State = #state{
  listen_socket = ListenSocket,
  loop = LoopFun
}) ->
  Self = self(),
  _PID = spawn(fun() -> acceptor_loop(Self, ListenSocket, LoopFun) end),
  State.

acceptor_loop(Server, ListenSocket, LoopFun) ->
  {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
  gen_server:cast(Server, {client_accepted, self()}),
  {Module, Function} = LoopFun,
  Module:Function(Server, ClientSocket).

handle_cast({client_accepted, _PID}, State) ->
  {noreply, accept(State)};

handle_cast({send, Sender, Message}, State = #state{clients = Clients}) ->
  SendFunction =
    fun({Receiver, _Username}, _) ->
      case Receiver =/= Sender of
        true -> gen_tcp:send(Receiver, Message);
        false -> ok
      end
    end,
  ets:foldl(SendFunction, none, Clients),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call({client_connected, ClientSocket, Username}, _From, State = #state{clients = Clients}) ->
  case ets:lookup(Clients, ClientSocket) of
    [] ->
      ets:insert(Clients, {ClientSocket, Username}),
      {reply, ok, State};
    [_Exists] ->
      {reply, already_exists, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.