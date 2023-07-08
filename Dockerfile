# Stage 1
FROM erlang:21-alpine AS build

#Creating working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy project code into workdir
COPY . /buildroot/web_rtp

# Coming into workdit
WORKDIR /buildroot/web_rtp

# Installing rebar3
# RUN wget -O rebar3 https://github.com/erlang/rebar3/releases/latest/download/rebar3 && \
# RUN wget -O rebar3 https://github.com/erlang/rebar3/releases/tag/3.10.0/download/rebar3 && \
#     chmod +x rebar3
RUN wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
RUN ./rebar3 local install
# RUN apk add --no-cache rebar3 
RUN apk add --no-cache make gcc musl-dev

# Downloading dependensies for project
RUN apk update && \
    apk add --no-cache ffmpeg ortp-dev bctoolbox-dev && \
    apk add --no-cache erlang 
    # && \
    # apk add --no-cache libncurses5-dev


# Installing git
RUN apk add git

# Compiling project with rebar3
RUN ./rebar3 as prod release

# Stage 2
FROM alpine:3.15

# Downloading deps for an operating system
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++


# Copy built app
COPY --from=build /buildroot/web_rtp/_build/prod/rel/web_rtp /web_rtp

# Exposing ports
EXPOSE 8080
EXPOSE 5060

# Stating app WebRTP
CMD ["/web_rtp/bin/web_rtp", "foreground"]
