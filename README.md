# rabbitmq-mqtt-retained-msg-redis

This plugin allows to store retained MQTT messages on Redis.

## Dependencies
- Exilier 1.15
- Erlang OTP 25
- Fork of wooga/eredis to support Redis Sentinel.
  - https://github.com/Nordix/eredis V1.7.1


## Docker build
The added Dockerfile needs to be adapated. Especially the version tags need to be aligned whith the set version in the erlang.mk file.

## Usage and contribution
Feel free to use and contribute

Under MIT License (MIT)