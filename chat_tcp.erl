-module(chat_tcp).

%% API
-export([start/1, send/2]).
%% Internal functions
-export([init/1]).

%%==============================================================================
%% API
%%==============================================================================
-spec start(pos_integer()) -> {ok, pid()}.
start(Port) ->
  Pid = erlang:spawn_link(?MODULE, init, [{Port}]),
  {ok, Pid}.

-spec send(pid() | atom(), binary()) -> ok.
send(ConnectionHandler, Message) ->
  % Create a unique reference for this request
  Ref = erlang:make_ref(),
  % Send the message to the process handling the TCP socket
  ConnectionHandler ! {send, self(), Ref, Message},
  receive
    % The unique reference makes sure we are only matching the response for this
    % request and nothing else
    {reply, Ref, Reply} ->
      Reply
    % TODO: Handle the case where the response never arrives
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================
init({Port}) ->
  % Start listening for incoming connections on the specified port. Keep in mind
  % that having a single process listening for new connections can easily become
  % a bottleneck, see the documentation for a potential solution:
  % https://erlang.org/doc/man/gen_tcp.html#accept-2
  {ok, ListenSocket} = gen_tcp:listen(Port, [binary,
                                             {active, true},
                                             {packet, 0}]),
  % Start the accept loop
  accept_loop(ListenSocket).

accept_loop(ListenSocket) ->
  % Accept a new connection (this call is blocking)
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  % Spawn a new process to handle the new connection
  Pid = erlang:spawn(fun() -> on_connection(Socket) end),
  % Change the owner of socket to the new process. This ensures this process now
  % receives the socket "events".
  ok = gen_tcp:controlling_process(Socket, Pid),
  % Contine accepting more clients
  accept_loop(ListenSocket).

on_connection(Socket) ->
  % Send an initial message to the client.
  % Please note that between spawning the function and the change of controlling
  % process change it is possible to lose some messages. In order to work around
  % this, we specified that the server will always send the initial message, for
  % our protocol, that would be the login prompt
  Message = <<"hello!">>,
  ok = gen_tcp:send(Socket, tcp_protocol:login_prompt(Message)),
  % TODO: Register this socket somewhere, somehow
  io:format("started handler ~p for incoming client~n", [self()]),
  % Start the receive loop
  receive_loop({Socket, <<>>}).

receive_loop({Socket, Buffer} = State) ->
  % Receive messages until the socket is closed or until the client missbehaves,
  % in either case, we just end the receive loop
  receive
    {tcp, Socket, Data} ->
      case tcp_protocol:parse_message(<<Buffer/binary, Data/binary>>) of
        {ok, NewBuffer, Message} ->
          % TODO: Do something with the message
          ok = io:format("Received ~p from client~n", [Message]),
          receive_loop({Socket, NewBuffer});
        {error, Reason} ->
          ok = io:format("Client dropped, reason: ~p~n", [Reason])
      end;
    {tcp_closed, Socket} ->
      % TODO: We should do something when the client closes the connection
      ok = io:format("Socket closed~n", []);
    {send, From, Ref, Message} ->
      ok = gen_tcp:send(Socket, Message),
      From ! {reply, Ref, ok},
      receive_loop(State)
  end.