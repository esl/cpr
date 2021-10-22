-module(tcp_protocol).

%% API
% Client API
-export([login/1,
         join_room/1, leave_room/1,
         send_room_message/2, send_direct_message/2,
         request_room_message_history/2]).
% Server API
-export([login_prompt/1, login_response/1,
         join_room_response/1, leave_room_response/0,
         send_room_message_response/1, send_direct_message_response/1,
         room_message_received/3, direct_message_received/2,
         room_message_history_response/1]).
% Utils
-export([parse_message/1]).

% C style binary types
-define(UCHAR, 8/unsigned-integer).
-define(USHORT, 16/unsigned-integer).
-define(UINT, 32/unsigned-integer).

% Payload types
% Note how the first bit distinguishes between client and server message and the
% second bit indicates whether it's a response or not
-define(LOGIN_PROMPT, 192).
-define(LOGIN, 1).
-define(LOGIN_RESPONSE, (128 + ?LOGIN)).
-define(JOIN_ROOM, 2).
-define(JOIN_ROOM_RESPONSE, (128 + ?JOIN_ROOM)).
-define(LEAVE_ROOM, 3).
-define(LEAVE_ROOM_RESPONSE, (128 + ?LEAVE_ROOM)).
-define(SEND_ROOM_MESSAGE, 4).
-define(SEND_ROOM_MESSAGE_RESPONSE, (128 + ?SEND_ROOM_MESSAGE)).
-define(ROOM_MESSAGE_RECEIVED, (192 + ?SEND_ROOM_MESSAGE)).
-define(SEND_DIRECT_MESSAGE, 5).
-define(SEND_DIRECT_MESSAGE_RESPONSE, (128 + ?SEND_DIRECT_MESSAGE)).
-define(DIRECT_MESSAGE_RECEIVED, (192 + ?SEND_DIRECT_MESSAGE)).
-define(REQUEST_ROOM_MESSAGE_HISTORY, 6).
-define(ROOM_MESSAGE_HISTORY_RESPONSE, (128 + ?REQUEST_ROOM_MESSAGE_HISTORY)).

% Maximum size for the buffer
-define(MAX_BUFFER_SIZE, 4096).

%%==============================================================================
%% Client API
%%==============================================================================
-spec login(iodata()) -> iodata().
login(UserId) ->
  [<<0:?UCHAR, ?LOGIN:?UCHAR,
     (erlang:iolist_size(UserId)):?USHORT>>, UserId].

-spec join_room(iodata()) -> iodata().
join_room(RoomName) ->
  [<<0:?UCHAR, ?JOIN_ROOM:?UCHAR,
     (erlang:iolist_size(RoomName)):?USHORT>>, RoomName].

-spec leave_room(iodata()) -> iodata().
leave_room(RoomName) ->
  [<<0:?UCHAR, ?LEAVE_ROOM:?UCHAR,
     (erlang:iolist_size(RoomName)):?USHORT>>, RoomName].

-spec send_room_message(iodata(), iodata()) -> iodata().
send_room_message(RoomName, Message) ->
  [<<0:?UCHAR, ?SEND_ROOM_MESSAGE:?UCHAR,
     (erlang:iolist_size(RoomName)):?USHORT>>, RoomName,
   <<(erlang:iolist_size(Message)):?USHORT>>, Message].

-spec send_direct_message(iodata(), iodata()) -> iodata().
send_direct_message(UserId, Message) ->
  [<<0:?UCHAR, ?SEND_DIRECT_MESSAGE:?UCHAR,
     (erlang:iolist_size(UserId)):?USHORT>>, UserId,
   <<(erlang:iolist_size(Message)):?USHORT>>, Message].

-spec request_room_message_history(iodata(), pos_integer()) -> iodata().
request_room_message_history(RoomName, MaxMessages) ->
  [<<0:?UCHAR, ?REQUEST_ROOM_MESSAGE_HISTORY:?UCHAR,
     (erlang:iolist_size(RoomName)):?USHORT>>, RoomName,
   <<MaxMessages:?UCHAR>>].

%%==============================================================================
%% Server API
%%==============================================================================
-spec login_prompt(iodata()) -> iodata().
login_prompt(Message) ->
  [<<0:?UCHAR, ?LOGIN_PROMPT:?UCHAR,
     (erlang:iolist_size(Message)):?USHORT>>, Message].

-spec login_response(iodata()) -> iodata().
login_response(Message) ->
  [<<0:?UCHAR, ?LOGIN_RESPONSE:?UCHAR,
     (erlang:iolist_size(Message)):?USHORT>>, Message].

-spec join_room_response(iodata()) -> iodata().
join_room_response(Message) ->
  [<<0:?UCHAR, ?JOIN_ROOM_RESPONSE:?UCHAR,
     (erlang:iolist_size(Message)):?USHORT>>, Message].

-spec leave_room_response() -> iodata().
leave_room_response() ->
  <<0:?UCHAR, ?LEAVE_ROOM_RESPONSE:?UCHAR>>.

-spec send_room_message_response(pos_integer()) -> iodata().
send_room_message_response(MessageId) ->
  <<0:?UCHAR, ?SEND_ROOM_MESSAGE_RESPONSE:?UCHAR, MessageId:?UINT>>.

-spec send_direct_message_response(pos_integer()) -> iodata().
send_direct_message_response(MessageId) ->
  <<0:?UCHAR, ?SEND_DIRECT_MESSAGE_RESPONSE:?UCHAR, MessageId:?UINT>>.

-spec room_message_history_response([{pos_integer(),
                                      string() | binary(),
                                      string() | binary()}]) -> iodata().
room_message_history_response(Messages) ->
  [<<0:?UCHAR, ?ROOM_MESSAGE_HISTORY_RESPONSE:?UCHAR,
     (erlang:length(Messages)):?UCHAR>>,
     [[<<MessageId:?UINT,
       (erlang:iolist_size(Sender)):?USHORT>>, Sender,
       <<(erlang:iolist_size(Message)):?USHORT>>, Message] ||
      {MessageId, Sender, Message} <- Messages]].

-spec room_message_received([string() | binary()],
                            [string() | binary()],
                            [string() | binary()]) -> iodata().
room_message_received(RoomName, Sender, Message) ->
  [<<0:?UCHAR, ?ROOM_MESSAGE_RECEIVED:?UCHAR,
     (erlang:iolist_size(RoomName)):?USHORT>>, RoomName,
   <<(erlang:iolist_size(Sender)):?USHORT>>, Sender,
   <<(erlang:iolist_size(Message)):?USHORT>>, Message].

-spec direct_message_received([string() | binary()],
                              [string() | binary()]) -> iodata().
direct_message_received(Sender, Message) ->
  [<<0:?UCHAR, ?DIRECT_MESSAGE_RECEIVED:?UCHAR,
     (erlang:iolist_size(Sender)):?USHORT>>, Sender,
   <<(erlang:iolist_size(Message)):?USHORT>>, Message].

%%==============================================================================
%% Utils
%%==============================================================================
-spec parse_message(binary()) -> {ok, binary(), any()} | {error, atom()}.
parse_message(<<_Flags:?UCHAR, ?LOGIN:?UCHAR,
                UserIdSize:?USHORT, UserId:UserIdSize/binary,
                Rest/binary>>) ->
  {ok, Rest, {login, erlang:binary_to_list(UserId)}};
parse_message(<<_Flags:?UCHAR, ?JOIN_ROOM:?UCHAR,
                RoomSize:?USHORT, Room:RoomSize/binary,
                Rest/binary>>) ->
  {ok, Rest, {join_room, erlang:binary_to_list(Room)}};
parse_message(<<_Flags:?UCHAR, ?LEAVE_ROOM:?UCHAR,
                RoomSize:?USHORT, Room:RoomSize/binary,
                Rest/binary>>) ->
  {ok, Rest, {leave_room, erlang:binary_to_list(Room)}};
parse_message(<<_Flags:?UCHAR, ?SEND_ROOM_MESSAGE:?UCHAR,
                RoomSize:?USHORT, Room:RoomSize/binary,
                MessageSize:?USHORT, Message:MessageSize/binary,
                Rest/binary>>) ->
  {ok, Rest, {send_room_message,
              erlang:binary_to_list(Room), erlang:binary_to_list(Message)}};
parse_message(<<_Flags:?UCHAR, ?SEND_DIRECT_MESSAGE:?UCHAR,
                UserIdSize:?USHORT, UserId:UserIdSize/binary,
                MessageSize:?USHORT, Message:MessageSize/binary,
                Rest/binary>>) ->
  {ok, Rest, {send_direct_message,
              erlang:binary_to_list(UserId), erlang:binary_to_list(Message)}};
parse_message(<<_Flags:?UCHAR, ?REQUEST_ROOM_MESSAGE_HISTORY:?UCHAR,
                RoomSize:?USHORT, Room:RoomSize/binary,
                MaxMessages:?UCHAR,
                Rest/binary>>) ->
  {ok, Rest, {request_room_message_history,
              erlang:binary_to_list(Room), MaxMessages}};
parse_message(<<_Flags:?UCHAR, ?LOGIN_PROMPT:?UCHAR,
                MessageSize:?USHORT, Message:MessageSize/binary,
                Rest/binary>>) ->
  {ok, Rest, {login_prompt, Message}};
parse_message(<<_Flags:?UCHAR, ?LOGIN_RESPONSE:?UCHAR,
                MessageSize:?USHORT, Message:MessageSize/binary,
                Rest/binary>>) ->
  {ok, Rest, {login_response, Message}};
parse_message(<<_Flags:?UCHAR, ?JOIN_ROOM_RESPONSE:?UCHAR,
                MessageSize:?USHORT, Message:MessageSize/binary,
                Rest/binary>>) ->
  {ok, Rest, {join_room_response, Message}};
parse_message(<<_Flags:?UCHAR, ?LEAVE_ROOM_RESPONSE:?UCHAR,
                Rest/binary>>) ->
  {ok, Rest, {leave_room_response}};
parse_message(<<_Flags:?UCHAR, ?SEND_ROOM_MESSAGE_RESPONSE:?UCHAR,
                MessageId:?UINT, Rest/binary>>) ->
  {ok, Rest, {send_room_message_response, MessageId}};
parse_message(<<_Flags:?UCHAR, ?SEND_DIRECT_MESSAGE_RESPONSE:?UCHAR,
                MessageId:?UINT, Rest/binary>>) ->
  {ok, Rest, {send_direct_message_response, MessageId}};
parse_message(<<_Flags:?UCHAR, ?ROOM_MESSAGE_RECEIVED:?UCHAR,
                RoomNameSize:?USHORT, RoomName:RoomNameSize/binary,
                UserIdSize:?USHORT, UserId:UserIdSize/binary,
                MessageSize:?USHORT, Message:MessageSize/binary,
                Rest/binary>>) ->
  {ok, Rest, {room_message_received, RoomName, UserId, Message}};
parse_message(<<_Flags:?UCHAR, ?DIRECT_MESSAGE_RECEIVED:?UCHAR,
                UserIdSize:?USHORT, UserId:UserIdSize/binary,
                MessageSize:?USHORT, Message:MessageSize/binary,
                Rest/binary>>) ->
  {ok, Rest, {direct_message_received, UserId, Message}};
parse_message(<<_Flags:?UCHAR, ?ROOM_MESSAGE_HISTORY_RESPONSE:?UCHAR,
                Messages:?UCHAR, Rest/binary>> = Bin) ->
  case parse_messages(Messages, Rest) of
    incomplete when byte_size(Bin) > ?MAX_BUFFER_SIZE ->
      {error, message_buffer_too_large};
    incomplete ->
      {ok, Bin, incomplete_message};
    {ok, NewRest, Messages} ->
      {ok, NewRest, {room_message_history_response, Messages}}
  end;
parse_message(NewBuffer) when byte_size(NewBuffer) < ?MAX_BUFFER_SIZE ->
  {ok, NewBuffer, incomplete_message};
parse_message(_LargeBuffer) ->
  {error, message_buffer_too_large}.

%%==============================================================================
%% Internal functions
%%==============================================================================
parse_messages(Count, Rest) ->
  parse_messages(Count, Rest, []).

parse_messages(0, Rest, Acc) ->
  {ok, Rest, lists:reverse(Acc)};
parse_messages(Count, <<MessageId:?UINT,
                        UserIdSize:?USHORT, UserId:UserIdSize/binary,
                        MessageSize:?USHORT, Message:MessageSize/binary,
                        Rest/binary>>, Acc) ->
  parse_messages(Count - 1, Rest, [{MessageId, UserId, Message} | Acc]);
parse_messages(_, _, _) ->
  incomplete.