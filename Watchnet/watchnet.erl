-module(watchnet).

-export([run/0, queue_loop/1, node/4, send_queue/2, queue/1, recv/2]).

-import(queue, [start/0]).

-include("constant.hrl").
-include("config.hrl").

queue_loop(Queue) ->
    receive
        {put, E} ->
            queue_loop(queue:in(E, Queue));
        {get, Pid} ->
            case queue:out(Queue) of
                {{value, Head}, NewQueue} -> 
                    Pid ! {ok, Head},
                    queue_loop(NewQueue);
                _ ->
                    Pid ! e,
                    queue_loop(Queue)
            end
    end.

node(Queue, Address, Sock, Remained)->
    case internet:recv(Sock, Remained) of
        {Request, Unprocessed} ->
            Queue ! {put, <<Address/binary, Request/binary>>},
            node(Queue, Address, Sock, Unprocessed);
        e ->
            % Report
            Queue ! {put, <<Address/binary, 0, 0>>},
            ets:delete(user, Address),
            ssl:close(Sock),
            exit(normal)
    end.

ip_tuple_to_binary({A, B, C, D}) ->
    <<A:8, B:8, C:8, D:8>>.

accept(Listen, Queue)->
    case internet:accept(Listen) of
        {ok, Node} ->
            {ok, {Address, _}} = ssl:peername(Node),
            Addr = ip_tuple_to_binary(Address),
            Connected = ets:member(user, Addr),
            if 
                byte_size(Addr) == 4 andalso Connected == false -> % IPv6 Later.
                    ets:insert(user, {Addr, Node}),
                    spawn(watchnet, node, [Queue, Addr, Node, <<>>]);
                true->
                    % Report
                    ssl:close(Node)
            end;
        {error, _} ->
            % Report
            _ = 0
    end,
    accept(Listen, Queue).

send_queue(Sock, Queue)->
    Queue ! {get, self()},
    receive
        {ok, Head} ->
            case internet:send(Sock, <<?FORWARD/binary, ?LOGS/binary, ?WRITE/binary, Head/binary>>) of
                ok ->
                    send_queue(Sock, Queue);
                e ->
                    % Report
                    Queue ! {put, Head},
                    e
            end;
        e ->
            send_queue(Sock, Queue)
    end.

recv(Sock, Remained)->
    case internet:recv(Sock, Remained) of
        {P, Unprocessed} ->
            <<_:8/binary, Source:4/binary, Suspicious:4/binary>> = P,
            case catch ets:lookup_element(user, Source, 2) of
                {'EXIT', _} ->
                    % Report
                    e;
                User ->
                    if
                        Suspicious == <<0, 1>> ->
                            ets:delete(user, Source),
                            ssl:close(User);
                        true ->
                            case internet:send(User, Suspicious) of
                                ok->
                                    ok;
                                e ->
                                    e
                            end
                        end
            end,
            recv(Sock, Unprocessed);
        e ->
            % Report
            exit(normal)
    end.

queue(Queue)->
    case internet:connect({127,0,0,1}, 7001, ?WATCHNET_CERT, ?WATCHNET_KEY) of
        {ok, Sock} ->
            internet:send(Sock, <<?CHANGE_NETWORK/binary, ?WATCHNET/binary>>),
            spawn(watchnet, recv, [Sock, <<>>]),
            send_queue(Sock, Queue);
        {error, _} ->
            % Report
            timer:sleep(4000)
    end,
    queue(Queue).

queue_create() ->
    Queue = queue:new(),
    spawn(watchnet, queue_loop, [Queue]).

run()->
    case ssl:start() of
        ok ->
            Queue = queue_create(),
            ets:new(user, [set, named_table, public]),
            spawn(logs, start, []),
            spawn(off, start, []),
            spawn(router, run, []),
            spawn(watchnet, queue, [Queue]),
            case internet:listen(9001, ?WATCHNET_CERT, ?WATCHNET_KEY) of
                {ok, Listen} ->
                    accept(Listen, Queue);
                {error, _} ->
                    % Report
                    ssl:stop(),
                    timer:sleep(4000),
                    run()
            end;
        {error, _} ->
            % Report
            e
    end.