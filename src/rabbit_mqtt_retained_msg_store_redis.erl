-module(rabbit_mqtt_retained_msg_store_redis).

-behaviour(rabbit_mqtt_retained_msg_store).

-include("deps/rabbitmq_mqtt/include/rabbit_mqtt_packet.hrl").
-include_lib("kernel/include/logger.hrl").

-export([new/2, recover/2, insert/3, lookup/2, delete/2, terminate/1]).

-record(store_state, {
    redis_client,
    key_prefix
}).

-type store_state() :: #store_state{}.

-spec retained_key_hash() -> binary().
retained_key_hash() ->
    "retained".

-spec retained_key(binary(), store_state()) -> binary().
retained_key(Topic, #store_state{key_prefix = Prefix}) ->
    retained_key_hash() ++ ":" ++ uri_string:quote(Prefix ++ "/" ++ Topic).

-spec convert_to_map(mqtt_msg_v0() | #mqtt_msg{}) -> map().
convert_to_map({mqtt_msg, Retain, Qos, Topic, Dup, Packet_id, Payload}) ->
    % Conversion for the mqtt_msg_v0 tuple
    mqtt_msg_v0_to_map({mqtt_msg, Retain, Qos, Topic, Dup, Packet_id, Payload});
convert_to_map(#mqtt_msg{
    retain = Retain,
    qos = Qos,
    topic = Topic,
    dup = Dup,
    packet_id = Packet_id,
    payload = Payload,
    props = Props,
    timestamp = Timestamp
}) ->
    % Conversion for the mqtt_msg record
    mqtt_msg_record_to_map(#mqtt_msg{
        retain = Retain,
        qos = Qos,
        topic = Topic,
        dup = Dup,
        packet_id = Packet_id,
        payload = Payload,
        props = Props,
        timestamp = Timestamp
    }).

-spec mqtt_msg_v0_to_map(mqtt_msg_v0()) -> map().
mqtt_msg_v0_to_map({RecordName, Retain, Qos, Topic, Dup, Packet_id, Payload}) ->
    % This is similar to what we had before for mqtt_msg_v0 to map conversion
    #{
        recordName => RecordName,
        retain => Retain,
        qos => Qos,
        topic => Topic,
        dup => Dup,
        packetId => Packet_id,
        payload => Payload
    }.

-spec mqtt_msg_record_to_map(#mqtt_msg{}) -> map().
mqtt_msg_record_to_map(#mqtt_msg{
    retain = Retain,
    qos = Qos,
    topic = Topic,
    dup = Dup,
    packet_id = Packet_id,
    payload = Payload,
    props = Props,
    timestamp = Timestamp
}) ->
    % Convert #mqtt_msg{} record to map
    #{
        retain => Retain,
        qos => Qos,
        topic => Topic,
        dup => Dup,
        packetId => Packet_id,
        payload => Payload,
        props => Props,
        timestamp => Timestamp
    }.

-spec encode_message(mqtt_msg_v0() | mqtt_msg()) -> binary().
encode_message(Message) ->
    jsx:encode(convert_to_map(Message)).

-spec json_to_mqtt_msg_v0(map()) -> mqtt_msg_v0().
json_to_mqtt_msg_v0(Map) ->
    {
        maps:get(recordName, Map),
        maps:get(retain, Map),
        maps:get(qos, Map),
        maps:get(topic, Map),
        maps:get(dup, Map),
        maps:get(packetId, Map),
        maps:get(payload, Map)
    }.

-spec json_to_mqtt_msg(map()) -> #mqtt_msg{}.
json_to_mqtt_msg(Map) ->
    #mqtt_msg{
        retain = maps:get(retain, Map),
        qos = maps:get(qos, Map),
        % Default to none if not present
        topic = maps:get(topic, Map, none),
        dup = maps:get(dup, Map),
        packet_id = maps:get(packetId, Map),
        payload = maps:get(payload, Map),
        % Default to empty map if not present
        props = maps:get(props, Map, #{}),
        % Default to none if not present
        timestamp = maps:get(timestamp, Map, none)
    }.

-spec decode_message(binary()) -> mqtt_msg().
decode_message(Pack) ->
    % Ensure maps are returned
    DecodedMap = jsx:decode(Pack, [{return_maps, true}]),
    Packet =
        case maps:get(recordName, DecodedMap, undefined) of
            % No recordName implies #mqtt_msg{}
            undefined -> json_to_mqtt_msg(DecodedMap);
            _ -> json_to_mqtt_msg_v0(DecodedMap)
        end,
    Packet.

-spec new(file:name_all(), rabbit_types:vhost()) -> store_state().
new(_, VHost) -> connect_to_redis(VHost).

-spec recover(file:name_all(), rabbit_types:vhost()) ->
    {error, uninitialized} | {ok, store_state()}.
recover(_, VHost) -> connect_to_redis(VHost).

-spec insert(binary(), mqtt_msg_v0() | mqtt_msg(), store_state()) -> ok.
insert(Topic, Message, #store_state{redis_client = Client, key_prefix = Prefix}) ->
    Key = retained_key(Topic, #store_state{key_prefix = Prefix}),
    {ok, RedisEnvTTL} = application:get_env(redis_msg_ttl),
    Pack = encode_message(Message),
    ?LOG_DEBUG("Inserting message to Redis in key ~p TTL: ~p ...", [
        Key, RedisEnvTTL
    ]),
    case catch (eredis:q(Client, ["SET", Key, Pack])) of
        {ok, _} ->
            eredis:q(Client, [
                "EXPIRE", Key, RedisEnvTTL
            ]),
            ok;
        {error, Reason} ->
            {error, Reason};
        {'EXIT', {timeout, _}} ->
            {error, timeout}
    end.

-spec lookup(binary(), store_state()) ->
    mqtt_msg() | undefined.
lookup(Topic, #store_state{redis_client = Client, key_prefix = Prefix}) ->
    Key = retained_key(Topic, #store_state{key_prefix = Prefix}),
    ?LOG_DEBUG("Getting message from Redis in key ~p...", [Key]),
    case catch (eredis:q(Client, ["GET", Key])) of
        {ok, undefined} ->
            undefined;
        {ok, Pack} ->
            decode_message(Pack);
        {error, Reason} ->
            ?LOG_ERROR("Error while reading redis key. Error message: ~p", [Reason]),
            undefined;
        {'EXIT', {timeout, _}} ->
            ?LOG_ERROR("Timeout while reading message"),
            undefined
    end.

-spec delete(binary(), store_state()) -> ok.
delete(Topic, #store_state{redis_client = Client, key_prefix = Prefix}) ->
    Key = retained_key(Topic, #store_state{key_prefix = Prefix}),
    {ok, ok} = eredis:q(Client, ["DEL", Key]).

-spec terminate(store_state()) -> ok.
terminate(#store_state{redis_client = Client}) ->
    ok = eredis:stop(Client).

connect_to_redis(VHost) ->
    {ok, RedisHost} = application:get_env(redis_host),
    {ok, RedisPort} = application:get_env(redis_port),
    {ok, RedisDatabase} = application:get_env(redis_database),
    {ok, RedisTTL} = application:get_env(redis_msg_ttl),
    ?LOG_INFO(
        "Connecting to ~p : ~p on database: ~p for VHost: ~p TTL: ~p...", [
            RedisHost, RedisPort, RedisDatabase, VHost, RedisTTL
        ]
    ),
    Options = [{host, RedisHost}, {port, RedisPort}, {database, RedisDatabase}],

    case eredis:start_link(Options) of
        {ok, Client} ->
            ?LOG_DEBUG(
                "Prefix: ~p for VHost: ~p", [
                    binary_to_list(VHost), VHost
                ]
            ),
            {ok, #store_state{redis_client = Client, key_prefix = binary_to_list(VHost)}};
        {error, Reason} ->
            {error, Reason}
    end.
