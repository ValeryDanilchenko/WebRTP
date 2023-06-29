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
    SupFlags = #{strategy => one_for_all,
                 intensity => 10,
                 period => 60},
    ChildSpecs = [
        #{
            id => web_rtp_nksip_sup,
            start => {web_rtp_nksip_sup, start_link, []},
            restart => transient,
            type => supervisor
        },
        #{
            id => web_rtp_db,
            start => {web_rtp_db, start, []},
            restart => transient,
            shutdown => 5000
        }],
    
    supervisor:start_link({local, ?MODULE}, ?MODULE, {SupFlags, ChildSpecs}).

%% @private
init(_ChildSpecs) ->
    {ok, _ChildSpecs}.


