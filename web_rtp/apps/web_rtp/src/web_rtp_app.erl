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
    Dispatch = cowboy_router:compile([
        {'_',[ 
            {"/", web_rtp_handler, []},
            {"/abonents", web_rtp_handler, []},
            {"/abonent/:abonent_number", web_rtp_handler, []},
            {"/abonent", web_rtp_handler, []}
        ]}
    ]),
    cowboy:start_clear(http,  [{port, 8080}], 
        #{env => #{dispatch => Dispatch}}),
    io:format("Application started. Cowboy HTTP server listening on http://localhost:8080~n"),

    web_rtp_sup:start_link().

%% @doc Callback function for stopping the application
%% @private
stop(_State) ->
    ok.
