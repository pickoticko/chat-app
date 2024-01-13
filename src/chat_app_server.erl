-module(chat_app_server).
-behaviour(gen_server).
-include("chat_app.hrl").

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

-define(DEFAULT_TCP_OPTIONS, [
  binary,
  {packet, 0},
  {active, false},
  {reuseaddr, true}
]).

-record(state, {
  handler = undefined,
  loop = undefined,
  listen_socket = null,
  port = null,
  ip = any
}).

start_link(ServerName, Port, LoopFun) ->
  logger:set_primary_config(level, info),
  State = #state{
    port = Port,
    loop = LoopFun
  },
  gen_server:start_link({local, ServerName}, ?MODULE, State, []).

init(State = #state{
  port = Port
}) ->
  ?LOGINFO("Listening for incoming connections..."),
  case gen_tcp:listen(Port, ?DEFAULT_TCP_OPTIONS) of
    {ok, ListenSocket} ->
      {ok, Handler} = chat_app_handler:start_link(),
      {ok, accept(State#state{
        listen_socket = ListenSocket,
        handler = Handler
      })};
    {error, Reason} ->
      {stop, Reason}
  end.

accept(State = #state{
  listen_socket = ListenSocket,
  loop = LoopFun,
  handler = Handler
}) ->
  Self = self(),
  spawn(fun() -> acceptor_loop(Self, Handler, ListenSocket, LoopFun) end),
  State.

acceptor_loop(Server, Handler, ListenSocket, LoopFun) ->
  {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
  gen_server:cast(Server, {client_accepted, ClientSocket}),
  gen_tcp:send(ClientSocket, "Connection accepted, waiting for join request..."),
  {Module, Function} = LoopFun,
  Module:Function(Handler, ClientSocket).

handle_cast({client_accepted, _ClientSocket}, State) ->
  {noreply, accept(State)};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.