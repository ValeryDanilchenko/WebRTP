# FROM erlang:24-alpine
FROM erlang:24-alpine AS build

#Переопределяем рабочую директорию
# RUN mkdir /buildroot
WORKDIR /buildroot

# Копирование кода проекта
COPY . /buildroot/web_rtp

# Переход в папку с проектом
WORKDIR /buildroot/web_rtp

# Установка rebar3
RUN apk add --no-cache make gcc musl-dev

# Обновление пакетного менеджера и установка необходимых зависимостей
RUN apk update && \
    apk add --no-cache ffmpeg ortp-dev bctoolbox-dev && \
    apk add --no-cache erlang
    # apk add --no-cache ffmpeg ortp-dev bctoolbox-dev && \
    # apk add --no-cache ncurses-libs && \
    # apk add --no-cache libstdc++



# Установка git
RUN apk add git

# Сборка проекта с помощью rebar3
RUN rebar3 compile
RUN rebar3 as prod release


FROM alpine:3.17

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

# Копирование собранного приложения из предыдущего образа
COPY --from=build /buildroot/web_rtp/_build/prod/rel/web_rtp /web_rtp

# Открытие портов (если необходимо)
EXPOSE 8080
EXPOSE 8443

# Точка входа для запуска приложения WebRTP
CMD ["/web_rtp/bin/web_rtp", "foreground"]
# CMD ["/web_rtp/bin/web_rtp", "rebar3 shell"]