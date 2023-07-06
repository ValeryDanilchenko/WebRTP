%%%-------------------------------------------------------------------
%% @doc web_rtp public API
%% @end
%%%-------------------------------------------------------------------

-module(web_rtp_app).
-author("ValeryDanilchenko").

-behaviour(application).

%% API
-export([start/2, stop/1]).

%% @doc Callback function for starting the application. Starts cowboy HTTP server 
%% and configures handlers for different routes.
%% @private
start(_Type, _StartArgs) -> 
    io:format("Application started!~n"),
    web_rtp_sup:start_link().

%% @doc Callback function for stopping the application
%% @private
stop(_State) ->
    ok.
