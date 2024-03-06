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

-spec new(file:name_all(), rabbit_types:vhost()) -> store_state().
new(_, VHost) -> connect_to_redis(VHost).


-spec recover(file:name_all(), rabbit_types:vhost()) ->
  {error, uninitialized} | {ok, store_state()}.
recover(_, VHost) -> connect_to_redis(VHost).

-spec insert(binary(), mqtt_msg(), store_state()) -> ok.
insert(Topic, Message, #store_state{redis_client = Client, key_prefix = Prefix}) ->
    Key = Prefix ++ "/" ++ Topic,
    {ok, RedisEnvTTL} = application:get_env(redis_msg_ttl),
    ?LOG_DEBUG("Inserting message ~p to Redis in key ~p TTL: ~p ...", [
        Message, Key, RedisEnvTTL
    ]),
    case catch (eredis:q(Client, ["SET", Key, Message])) of
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
    Key = Prefix ++ "/" ++ Topic,
    ?LOG_DEBUG("Getting message from Redis in key ~p...", [Key]),
    case catch (eredis:q(Client, ["GET", Key])) of
        {ok, undefined} -> undefined;
        {ok, Message} -> binary_to_term(Message);
        {error, Reason} -> ?LOG_ERROR("Error while reading redis key. Error message: ~p", [Reason]), undefined;
        {'EXIT', {timeout, _}} -> ?LOG_ERROR("Timeout while reading message"), undefined
    end.

-spec delete(binary(), store_state()) -> ok.
delete(Topic, #store_state{redis_client = Client, key_prefix = Prefix}) ->
    Key = Prefix ++ "/" ++ Topic,
    {ok, ok} = eredis:q(Client, ["DEL", Key]).

-spec terminate(store_state()) -> ok.
terminate(#store_state{}) ->
    {ok, ok}.

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
    Options = [{host, RedisHost}, {port, RedisPort}, {database, RedisDatabase}]
    ,
    case eredis:start_link(Options) of
        {ok, Client} ->
            {ok, #store_state{redis_client = Client, key_prefix = binary_to_list(VHost)}};
        {error, Reason} ->
            {error, Reason}
    end.
