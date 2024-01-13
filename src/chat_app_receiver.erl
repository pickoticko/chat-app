-module(chat_app_receiver).
-include("chat_app.hrl").

-export([init/2]).

-record(client, {
  handler,
  socket
}).

init(Handler, Socket) ->
  init(#client{
    handler = Handler,
    socket = Socket
  }).

init(State = #client{
  handler = Handler,
  socket = Socket
}) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Packet} ->
      case Packet of
        <<"connect:", Username/binary>> ->
          case gen_server:call(Handler, {connect, Socket, Username}) of
            %% User is accepted, enter the loop
            ok ->
              loop(State);
            %% User is rejected, already in the chat
            {error, _Error} ->
              init(State)
          end;
        _UnexpectedMessage ->
          init(State)
      end;
    {error, closed} ->
      ok
  end.

loop(State = #client{
  socket = Socket
}) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Packet} ->
      handle_packet(Packet, State),
      loop(State);
    {error, closed} ->
      ok
  end.

handle_packet(<<"send:", Message/binary>>, #client{
  handler = Handler,
  socket = Socket
}) ->
  gen_server:cast(Handler, {send, Socket, Message});

handle_packet(<<"disconnect">>, #client{
  handler = Handler,
  socket = Socket
}) ->
  case gen_server:call(Handler, {disconnect, Socket}) of
    ok -> ok;
    already_disconnected -> ok
  end;

handle_packet(UnexpectedCommand, #client{
  socket = Socket
}) ->
  ?LOGWARNING("[~p] sent unexpected command: ~p", [Socket, UnexpectedCommand]).
