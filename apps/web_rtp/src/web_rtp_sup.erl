%%%-------------------------------------------------------------------
%% @doc web_rtp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(web_rtp_sup).
-author("ValeryDanilchenko").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Callback
-export([init/1]).

-define(SERVER, ?MODULE).


%% @doc API function starting supervisor process
%% @private
start_link() ->
    start_cowboy(),    
    SupFlags = #{strategy => one_for_all,
                 intensity => 10,
                 period => 60},
    ChildSpecs = [
        nksip:get_sup_spec(test_ip_102, #{
            sip_from => "sip:102@test.domain",
            plugins => [nksip_uac_auto_auth, nksip_100rel],
            sip_listen => "<sip:all:5060;transport=udp>"
        }),
        #{
            id => web_rtp_db,
            start => {web_rtp_db, start, []},
            restart => transient,
            shutdown => 5000
        }
    ],
    
    supervisor:start_link({local, ?MODULE}, ?MODULE, {SupFlags, ChildSpecs}).

%% @doc API function starting cowboy
%% @private
start_cowboy() ->
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
    io:format("Cowboy HTTP server listening on http://localhost:8080~n").

%% @private
init(_ChildSpecs) ->
    {ok, _ChildSpecs}.


