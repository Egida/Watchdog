% Payload Header
-define(LENGTH, 2).
-define(SEQUENCE_NUMBER, 4).
-define(CHECKSUM, 8).
-define(TIMESTAMP, 8).
-define(PAYLOAD_HEADER, ?LENGTH + ?SEQUENCE_NUMBER + ?CHECKSUM + ?TIMESTAMP).

% 0x64 Commands
-define(FORWARD, <<0, 0>>).
-define(CHANGE_NETWORK, <<0, 1>>).
-define(INFO, <<0, 2>>).
-define(UP, <<0, 3>>).

% 0x64 Constant
-define(DEST, 8).

% Database
-define(SOURCE, 4).
% -define(CHECKSUM, 8).

% Packet
-define(MAXIMUM_PACKET_LENGTH, 65535).
-define(ROUTING_RESERVERD, 12).
-define(MAXIMUM_INCOMING_LENGTH, ?MAXIMUM_PACKET_LENGTH - ?LENGTH - ?ROUTING_RESERVERD).

-define(MINIMUM_PACKET_LENGTH, ?PAYLOAD_HEADER).
-define(MINIMUM_INCOMING_LENGTH, ?MINIMUM_PACKET_LENGTH - ?LENGTH).