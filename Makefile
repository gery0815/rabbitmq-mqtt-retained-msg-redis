PROJECT = rabbitmq_mqtt_retained_msg_store_redis
PROJECT_DESCRIPTION = RabbitMQ MQTT Plugin to store retained messages into Redis
PROJECT_MOD = rabbitmq_mqtt_retained_msg_store_redis

define PROJECT_ENV
[
		{redis_host, <<"localhost">>},
		{redis_port, 6379},
		{redis_database, 5},
		{redis_msg_ttl, 1209600}
	  ]
endef

DEPS = rabbit_common rabbit amqp_client rabbitmq_mqtt eredis
TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

PROJECT_VERSION = v3.12.13

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = v3.12.13

include rabbitmq-components.mk
include erlang.mk
