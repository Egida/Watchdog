-module(internet).

-include("constant.hrl").

-export([listen/3, listen_local/3, connect/4, accept/1, recv/2, send/2, unix_timestamp/0]).

unix_timestamp() ->
    {MegaSecs, Secs, _} = os:timestamp(),
    MegaSecs * 1000000 + Secs.
sha2_512_checksum(Input, _length) ->
    _hash = crypto:hash(sha256, Input),
    <<_checksum:_length/binary, _/binary>> = _hash,
    _checksum.

send(Sock, _response)->
    _length_integer = ?PAYLOAD_HEADER - ?LENGTH + byte_size(_response),
    _length = <<(_length_integer bsr 8):8, _length_integer:8>>,

    _sequence_number = <<0, 0, 0, 0>>,  % ToDo: Make it Functional.
    
    _checksum = sha2_512_checksum(_response, 8),

    _timestamp_integer = unix_timestamp(),
    _timestamp = <<(_timestamp_integer bsr 56):8, (_timestamp_integer bsr 48):8, (_timestamp_integer bsr 40):8, (_timestamp_integer bsr 32):8, 
        (_timestamp_integer bsr 24):8, (_timestamp_integer bsr 16):8, (_timestamp_integer bsr 8):8, _timestamp_integer:8>>,
    
    _payload = <<_length/binary, _sequence_number/binary, _checksum/binary, _timestamp/binary , _response/binary>>,

    case ssl:send(Sock, _payload) of
        ok ->
            ok;
        {error, _} ->
            % Report
            e
    end.

validity(P)->
    <<_sequence_number:?SEQUENCE_NUMBER/binary, _recvd_checksum:?CHECKSUM/binary, _recvd_timestamp:?TIMESTAMP/binary, _request/binary>> = P,

    _computed_checksum = sha2_512_checksum(<<_request/binary>>, 8),
    _recvd_timestamp_int = binary:decode_unsigned(_recvd_timestamp),
    _current_timestamp = unix_timestamp(),
    if 
        _computed_checksum =/= _recvd_checksum orelse _current_timestamp - _recvd_checksum >= 2 orelse _current_timestamp - _recvd_timestamp_int < 0 ->
            e;
        true ->
            {ok, <<_request/binary>>}
    end.

process_remained(Sock, _remained_unprocessed_binary)->
    if
        byte_size(_remained_unprocessed_binary) >= ?LENGTH ->
            Length = ?LENGTH * 8,
            <<_incoming_request_length:Length, _unprocessed_binary/binary>> = _remained_unprocessed_binary,
            if 
                _incoming_request_length >= ?MINIMUM_INCOMING_LENGTH andalso _incoming_request_length =< ?MAXIMUM_INCOMING_LENGTH ->
                    recv(Sock, _unprocessed_binary, byte_size(_unprocessed_binary), _incoming_request_length);
                true ->
                    % Report
                    e
            end;
        true ->
            % Report
            e
    end.

recv(_, _unprocessed_binary, _unprocessed_binary_size, _incoming_request_length) when _unprocessed_binary_size >= _incoming_request_length ->
    <<P:_incoming_request_length/binary, _remained_unprocessed_binary/binary >> = _unprocessed_binary,
    case validity(P) of
        {ok, _request} ->
            {_request, _remained_unprocessed_binary};
        e ->
            % Report Watchdog
            e
    end;

recv(Sock, _unprocessed_binary, _unprocessed_binary_size, _incoming_request_length)->
    case ssl:recv(Sock, 0) of
        {ok, _new_received_binary} ->
            recv(Sock, <<_unprocessed_binary/binary, _new_received_binary/binary>>, _unprocessed_binary_size + byte_size(_new_received_binary), _incoming_request_length);
        {error, _} ->
            % Report
            e
    end.

recv(Sock, _remained_unprocessed_binary)->
    case process_remained(Sock, _remained_unprocessed_binary) of
        e ->
            case ssl:recv(Sock, 0) of
                {ok, _new_received_binary} ->
                    _new_unprocessed_binary = <<_remained_unprocessed_binary/binary, _new_received_binary/binary>>,
                    if
                        byte_size(_new_unprocessed_binary) >= ?LENGTH ->
                            Length = ?LENGTH * 8,
                            <<_incoming_request_length:Length, _unprocessed_binary/binary>> = _new_unprocessed_binary,
                            if 
                                _incoming_request_length >= ?MINIMUM_INCOMING_LENGTH andalso _incoming_request_length =< ?MAXIMUM_INCOMING_LENGTH ->
                                    recv(Sock, _unprocessed_binary, byte_size(_unprocessed_binary), _incoming_request_length);
                                true ->
                                    % Report
                                    e
                            end;
                        true ->
                            % Report
                            e
                    end;
                {error, _} ->
                    % Report
                    e
            end;
        P ->
            P
    end.

connect(Address, Port, Cert, Key)->
    ssl:connect(Address, Port, [   {certfile, Cert}, 
                                        {keyfile, Key}, 
                                        {verify, verify_none}, {active, false}, {mode, binary}], infinity).
accept(L) ->
    case ssl:transport_accept(L) of
        {ok, Socket} ->
            ssl:handshake(Socket);
        {error, _} ->
            e
    end.

listen_local(Port, Cert, Key)->
    ssl:listen(Port, [{certfile, Cert}, {keyfile, Key}, {active, false}, {mode, binary}, {reuseaddr, true}, {ip, {127,0,0,1}}]).

listen(Port, Cert, Key) ->
    ssl:listen(Port, [{certfile, Cert}, {keyfile, Key}, {active, false}, {mode, binary}, {reuseaddr, true}]).