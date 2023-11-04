% Payload Header
-define(LENGTH, 2).
-define(SEQUENCE_NUMBER, 4).
-define(CHECKSUM, 8).
-define(TIMESTAMP, 8).
-define(PAYLOAD_HEADER, ?LENGTH + ?SEQUENCE_NUMBER + ?CHECKSUM + ?TIMESTAMP).

% 0x64 Constant
-define(DEST, 8).

-define(SOURCE_IP, 4).
-define(SOURCE_PORT, 2).
-define(DESTINATION_IP, 4).
-define(DESTINATION_PORT, 2).
% -define(TIMESTAMP, 8).
-define(REQUEST, 2).
-define(MINIMUM_PACKET_INFO_LENGTH, ?SOURCE_IP + ?SOURCE_PORT + ?DESTINATION_IP + ?DESTINATION_PORT + ?TIMESTAMP + ?REQUEST).

% 0x64 Commands
-define(FORWARD, <<0, 0>>).
-define(CHANGE_NETWORK, <<0, 1>>).

% Database
-define(SOURCE, 4).
% -define(CHECKSUM, 8).

% Database Commands
-define(WRITE, <<0, 0>>).
-define(DELETE, <<0, 1>>).
-define(WIPE, <<0, 2>>).
-define(READ_FROM_BEGINNING, <<0, 3>>).
-define(READ_NEXT, <<0, 4>>).
-define(READ_END, <<0, 5>>).
-define(MEMBER, <<0, 6>>).
-define(INFO, <<0, 7>>).

% Packet
-define(MAXIMUM_PACKET_LENGTH, 65535).
-define(ROUTING_RESERVERD, 12).
-define(MAXIMUM_INCOMING_LENGTH, ?MAXIMUM_PACKET_LENGTH - ?LENGTH - ?ROUTING_RESERVERD).

-define(MINIMUM_PACKET_LENGTH, ?PAYLOAD_HEADER).
-define(MINIMUM_INCOMING_LENGTH, ?MINIMUM_PACKET_LENGTH - ?LENGTH).