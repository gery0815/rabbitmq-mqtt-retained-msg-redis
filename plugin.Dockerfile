FROM elixir:1.15-otp-25 as builder

WORKDIR /app

# install missing deps
RUN apt-get update
RUN apt-get install zip -y
RUN apt-get install rsync -y

# copy the enriched rabbitmq repos
COPY ["./*", "/app/"]

WORKDIR /app

RUN echo "run erlang app"
RUN make
RUN make dist
WORKDIR /app/archiver
RUN cp -r /app/plugins/rabbitmq_mqtt_retained_msg_store_redis-3.12.4 ./rabbitmq_mqtt_retained_msg_store_redis-3.12.4
RUN cp -r /app/plugins/eredis-1.7.1 ./

# create archive files
RUN zip -r rabbitmq_mqtt_retained_msg_store_redis-3.12.4.ez rabbitmq_mqtt_retained_msg_store_redis-3.12.4
RUN zip -r eredis-1.7.1.ez eredis-1.7.1

#cleanup directory
RUN rm -rf rabbitmq_mqtt_retained_msg_store_redis-3.12.4
RUN rm -rf eredis-1.7.1

FROM scratch AS binaries
COPY --from=builder /app/archiver/ /bin
