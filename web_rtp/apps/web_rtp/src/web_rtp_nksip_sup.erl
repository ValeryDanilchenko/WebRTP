-module(web_rtp_nksip_sup).
-author("ValeryDanilchenko").

-behaviour(supervisor).

-export([init/1, start_link/0]).

%% @private
start_link() ->
    ChildsSpec = [
        nksip:get_sup_spec(test_ip_102, #{
            sip_from => "sip:102@test.domain",
            plugins => [nksip_uac_auto_auth, nksip_100rel],
            sip_listen => "<sip:all:5060;transport=udp>"
        })
        % nksip:get_sup_spec(test_ip_104, #{
        %     sip_from => "sip:104@test.domain",
        %     plugins => [nksip_uac_auto_auth, nksip_100rel],
        %     sip_listen => "<sip:all:5070;transport=udp>"
        % }),
        % nksip:get_sup_spec(test_ip_105, #{
        %     sip_from => "sip:105@test.domain",
        %     plugins => [nksip_uac_auto_auth, nksip_100rel],
        %     sip_listen => "<sip:all:5080;transport=udp>"
        % })
    ],
    supervisor:start_link({local, ?MODULE}, ?MODULE, {{one_for_one, 10, 60}, ChildsSpec}).

%% @private
init(ChildSpecs) ->
    {ok, ChildSpecs}.

