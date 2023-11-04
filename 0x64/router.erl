-module(router).

-include("constant.hrl").
-include("config.hrl").

-export([run/0, node/3]).

forward(X64, Dest, Message)->
    case catch ets:lookup_element(connections, Dest, 2) of
        {'EXIT', _} ->
            % Report
            e;
        Sock ->
            case internet:send(Sock, <<X64/binary, Message/binary>>) of
                ok->
                    ok;
                e ->
                    e
            end
    end.

change_network(Next, Prev)->
    if
        byte_size(Next) == 8 ->
            case ets:lookup(connections, Next) of
                [] ->
                    case ets:lookup(connections, Prev) of
                        [{_, Node, Address}]->
                            ets:delete(connections, Prev),
                            ets:insert(connections, {Next, Node, Address}),
                            Next;
                        _ ->
                            % Report
                            Prev
                    end;
                _ ->
                    % Report
                    Prev
            end;
        true ->
            % Report
            Prev
    end.

up(X64)->
    if
        byte_size(X64) == 8 ->
            case ets:lookup(connections, X64) of
                [] ->
                    <<0, 1>>;
                _ ->
                    <<0, 0>>
            end;
        true ->
            % Report
            <<0, 1>>
    end.

node(Sock, Remained, X64)->
    case internet:recv(Sock, Remained) of
        {Request, Unprocessed} ->
            <<Command:2/binary, Body/binary>> = Request,
            case Command of
                ?FORWARD ->
                    <<Dest:?DEST/binary, Message/binary>> = Body,
                    forward(X64, Dest, Message);
                ?CHANGE_NETWORK ->
                    Next = change_network(Body, X64),
                    node(Sock, Unprocessed, Next);
                ?INFO ->
                    internet:send(Sock, X64);
                ?UP ->
                    internet:send(Sock, up(Body));
                _ ->
                    % Report
                    _ = 0
            end,
            node(Sock, Unprocessed, X64);
        e ->
            ets:delete(connections, X64),
            ssl:close(Sock),
            exit(normal)
    end.

ip_tuple_to_binary({A, B, C, D}) ->
    <<A:8, B:8, C:8, D:8>>.

accept(Listen)->
    case internet:accept(Listen) of
        {ok, Node} ->
            {ok, {Address, _}} = ssl:peername(Node),
            if 
                tuple_size(Address) == 4 -> % IPv6 Later.
                    X64 = crypto:strong_rand_bytes(8),
                    ets:insert(connections, {X64, Node, ip_tuple_to_binary(Address)}),
                    spawn(router, node, [Node, <<>>, X64]);
                true->
                    % Report
                    ssl:close(Node)
            end,
            accept(Listen);
        {error, E} ->
            % Report
            accept(Listen)
    end.

run()->
    case ssl:start() of
        ok ->
            ets:new(connections, [set, named_table, public]),
            case internet:listen_local(7001, ?ROUTER_CERT, ?ROUTER_KEY) of
                {ok, Listen} ->
                    accept(Listen);
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