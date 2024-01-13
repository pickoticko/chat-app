-module(chat_app_receiver).

%% API
-export([init/2]).

init(Handler, ClientSocket) ->
  case gen_tcp:recv(ClientSocket, 0) of
    {ok, Packet} ->
      case Packet of
        <<"connect:", Username/binary>> ->
          case gen_server:call(Handler, {connect, ClientSocket, Username}) of
            ok ->
              loop(Handler, ClientSocket);
            {error, _Error} ->
              init(Handler, ClientSocket)
          end;
        _UnexpectedMessage ->
          init(Handler, ClientSocket)
      end;
    {error, closed} ->
      ok
  end.

loop(Handler, ClientSocket) ->
  case gen_tcp:recv(ClientSocket, 0) of
    {ok, Packet} ->
      case Packet of
        <<"send:", Message/binary>> ->
          io:format("Client ~p sent the message: ~p~n", [ClientSocket, binary_to_list(Message)]),
          gen_server:cast(Handler, {send, ClientSocket, Message}),
          loop(Handler, ClientSocket);
        <<"leave">> ->
          case gen_server:call(Handler, {disconnect, ClientSocket}) of
            ok -> ok;
            already_disconnected -> ok
          end
      end;
    {error, closed} ->
      ok
  end.