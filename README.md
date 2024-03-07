# rabbitmq-mqtt-retained-msg-redis

This plugin allows to store retained MQTT messages on Redis.

## Dependencies
- Exilier 1.15
- Erlang OTP 25
- Fork of wooga/eredis to support Redis Sentinel.
  - https://github.com/Nordix/eredis V1.7.1

## Usage
Build the plugin create .ez archive file and copy it into plugins directory: "opt/bitnami/rabbitmq/plugins/"

## Environment variables

The following setting enables the redis plugin.
  - mqtt.retained_message_store = rabbit_mqtt_retained_msg_store_redis

We use the following rabbitmq env variables to set the connection properties.
  - mqtt.redis_host = redis-host
  - mqtt.redis_port = 6379
  - mqtt.redis_database = 5
  - mqtt.redis_msg_ttl = 1209600


## Docker build
The added Dockerfile needs to be adapated. Especially the version tags need to be aligned whith the set version in the erlang.mk file.

## Usage and contribution
Feel free to use and contribute

Under MIT License (MIT)
