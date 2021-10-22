-module(chat_client).

%% API
-export([start/2]).
-export([login/2,
         join_room/2, leave_room/2,
         send_room_message/3, send_direct_message/3,
         request_room_message_history/3]).
%% Internal functions
-export([init/1]).

%%==============================================================================
%% API
%%==============================================================================
-spec start(string(), pos_integer()) -> {ok, pid()}.
start(Address, Port) ->
  Pid = erlang:spawn_link(?MODULE, init, [{Address, Port}]),
  {ok, Pid}.

-spec login(pid() | atom(), iodata()) -> ok.
login(ClientRef, UserId) ->
  ClientRef ! {send, login, [UserId]},
  ok.

-spec join_room(pid() | atom(), iodata()) -> ok.
join_room(ClientRef, RoomName) ->
  ClientRef ! {send, join_room, [RoomName]},
  ok.

-spec leave_room(pid() | atom(), iodata()) -> ok.
leave_room(ClientRef, RoomName) ->
  ClientRef ! {send, leave_room, [RoomName]},
  ok.

-spec send_room_message(pid() | atom(), iodata(), iodata()) -> ok.
send_room_message(ClientRef, RoomName, Message) ->
  ClientRef ! {send, send_room_message, [RoomName, Message]},
  ok.

-spec send_direct_message(pid() | atom(), iodata(), iodata()) -> ok.
send_direct_message(ClientRef, UserId, Message) ->
  ClientRef ! {send, send_direct_message, [UserId, Message]},
  ok.

-spec request_room_message_history(pid() | atom(), iodata(), pos_integer()) ->
  ok.
request_room_message_history(ClientRef, RoomName, MaxMessages) ->
  ClientRef ! {send, request_room_message_history, [RoomName, MaxMessages]},
  ok.

%%==============================================================================
%% Internal functions
%%==============================================================================
init({Address, Port}) ->
  {ok, Socket} = gen_tcp:connect(Address, Port, [binary,
                                                 {active, true},
                                                 {packet, 0}]),
  % Start the receive loop
  loop({Socket, <<>>}).

loop({Socket, Buffer} = State) ->
  receive
    {tcp, Socket, Data} ->
      case tcp_protocol:parse_message(<<Buffer/binary, Data/binary>>) of
        {ok, NewBuffer, Message} ->
          ok = io:format("Received ~p from server~n", [Message]),
          loop({Socket, NewBuffer});
        {error, Reason} ->
          ok = io:format("Unexpected parse error, reason: ~p~n", [Reason])
      end;
    {tcp_closed, Socket} ->
      ok = io:format("Socket closed~n", []);
    {login, UserId} ->
      Msg = tcp_protocol:login(UserId),
      ok = gen_tcp:send(Socket, Msg),
      loop(State);
    {send, MessageType, Args} ->
      Msg = erlang:apply(tcp_protocol, MessageType, Args),
      ok = gen_tcp:send(Socket, Msg),
      loop(State)
  end.