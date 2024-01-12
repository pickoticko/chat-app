-module(chat_app_receiver).

%% API
-export([init/2]).

init(Server, ClientSocket) ->
  case gen_tcp:recv(ClientSocket, 0) of
    {ok, Packet} ->
      case Packet of
        <<"connect:", Username/binary>> ->
          case gen_server:call(Server, {client_connected, ClientSocket, Username}) of
            ok ->
              io:format("Client joined the chat: ~p~n", [Username]),
              loop(Server, ClientSocket);
            already_exists ->
              gen_tcp:send(ClientSocket, "User already in the chat!"),
              init(Server, ClientSocket)
          end;
        _UnexpectedMessage ->
          init(Server, ClientSocket)
      end;
    {error, closed} ->
      ok
  end.

loop(Server, ClientSocket) ->
  case gen_tcp:recv(ClientSocket, 0) of
    {ok, Packet} ->
      case Packet of
        <<"send:", Message/binary>> ->
          io:format("Client ~p sent the message: ~p~n", [ClientSocket, binary_to_list(Message)]),
          gen_server:cast(Server, {send, ClientSocket, Message});
        <<"leave">> ->
          todo
      end,
      loop(Server, ClientSocket);
    {error, closed} ->
      ok
  end.