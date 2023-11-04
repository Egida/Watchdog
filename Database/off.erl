-module(off).

-import(internet, [listen/3]).

-export([start/0, dbops/2]).

-include("constant.hrl").
-include("config.hrl").

delete(Hash)->
    mnesia:transaction(
        fun() ->
            mnesia:delete({off, Hash})
        end).

wipe_from_beginning() ->
    case catch mnesia:first(off) of
        '$end_of_table' ->
            <<0, 0>>;
        Key ->
            case catch mnesia:next(off, Key) of
                {'EXIT', _} ->
                    delete(Key),
                    <<0, 0>>;
                '$end_of_table' ->
                    delete(Key),
                    <<0, 0>>;
                Next ->
                    delete(Key),
                    Next
            end
    end.

wipe_next(Prev) ->
    mnesia:transaction(fun()->
        case catch mnesia:next(off, Prev) of
            {'EXIT', _} ->
                wipe_from_beginning();
            '$end_of_table' ->
                delete(Prev),
                <<0, 0>>;
            Key ->
                case catch mnesia:next(off, Key) of
                    {'EXIT', _} ->
                        delete(Key),
                        <<0, 0>>;
                    '$end_of_table' ->
                        delete(Key),
                        <<0, 0>>;
                    Next ->
                        delete(Key),
                        Next
                end
        end
    end).

wipe(_, 0)->
    <<0, 0>>;

wipe(Prev, N)->
    {atomic, Next} = wipe_next(Prev),
    wipe(Next, N-1).

read_end() ->
    mnesia:transaction(fun()->
        case mnesia:last(off) of
            '$end_of_table' ->
                {<<>>, atom_to_binary('$end_of_table')};
            Key ->
                case mnesia:read(off, Key) of
                    [{off, _, Log}] ->
                        Log;
                    _ ->
                        read_end()
                end
        end
    end).

read_from_beginning() ->
    mnesia:transaction(fun() ->
        case catch mnesia:first(off) of
            '$end_of_table' ->
                {<<>>, atom_to_binary('$end_of_table')};
            Key ->
                case mnesia:read(off, Key) of
                    [{off, _, Log}]->
                        Log;
                    []->
                        read_from_beginning()
                end
        end
    end).

read_again() ->
    case catch mnesia:first(off) of
        '$end_of_table' ->
            {<<>>, atom_to_binary('$end_of_table')};
        Key ->
            case mnesia:read(off, Key) of
                [{off, _, Log}]->
                    Log;
                []->
                    read_again()
            end
    end.

read_next(Prev) ->
    mnesia:transaction(fun() ->
        case catch mnesia:next(off, Prev) of
            {'EXIT', _} ->
                read_again();
            '$end_of_table' ->
                {Prev, atom_to_binary('$end_of_table')};
            Next ->
                case mnesia:read(off, Next) of
                    [{off, _, Log}]->
                        Log;
                    []->
                        read_next(Prev)
                end
        end
    end).

member(Address)->
    {atomic, R} = mnesia:transaction(fun()->
        case mnesia:read(off, Address) of
            [_]->
                <<0, 0>>;
            []->
                <<0, 1>>
        end
    end),
    R.

write(Log) ->
    <<Checksum:?CHECKSUM/binary, _/binary>> = crypto:hash(blake2b, Log),
    mnesia:transaction(fun() ->
        mnesia:write({off, Checksum, Log})
    end).

dbops(Sock, Remained)->
    case internet:recv(Sock, Remained) of
        {Request, Unprocessed} ->
            <<X64:8/binary, Command:2/binary, Body/binary>> = Request,
            case Command of
                ?WRITE ->
                    write(Body);
                ?READ_FROM_BEGINNING ->
                    {atomic, Log} = read_from_beginning(),
                    internet:send(Sock, <<?FORWARD/binary, X64/binary, Log/binary>>);
                ?READ_NEXT ->
                    {atomic, Log} = read_next(Body),
                    internet:send(Sock, <<?FORWARD/binary, X64/binary, Log/binary>>);
                ?READ_END ->
                    {atomic, Log} = read_end(),
                    internet:send(Sock, <<?FORWARD/binary, X64/binary, Log/binary>>);
                ?DELETE ->
                    delete(Body);
                ?WIPE ->
                    R = wipe(<<>>, binary:decode_unsigned(Body)),
                    internet:send(Sock, <<?FORWARD/binary, X64/binary, R/binary>>);
                ?MEMBER ->
                    R = member(Body),
                    internet:send(Sock, <<?FORWARD/binary, X64/binary, R/binary>>);
                ?INFO ->
                    Info = binary:encode_unsigned(mnesia:table_info(off, size)),
                    internet:send(Sock, <<?FORWARD/binary, X64/binary, Info/binary>>);
                _ ->
                    % Report
                    _ = 0
            end,
            dbops(Sock, Unprocessed);
        e ->
            % Report
            start()
    end.

setup() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(off, [{disc_copies, [node()]}, {attributes, [checksum, addr]}]).

start()->
    setup(),
    case ssl:start() of
        ok ->
            case internet:connect({127,0,0,1}, 7001, ?OFF_CERT, ?OFF_KEY) of
                {ok, Sock} ->
                    internet:send(Sock, <<?CHANGE_NETWORK/binary, ?OFF/binary>>),
                    dbops(Sock, <<>>);
                {error, _} ->
                    % Report
                    timer:sleep(4000),
                    start()
            end;
        {error, _} ->
            start()
    end.