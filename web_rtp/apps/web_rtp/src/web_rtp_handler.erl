-module(web_rtp_handler).
-author("ValeryDanilchenko").

-include_lib("nkserver/include/nkserver_callback.hrl").
-include_lib("nksip/include/nksip.hrl").

-export([init/2, call_abonent/1]).


init(Req, State) ->
    io:format("Handler was called!~nReq: ~p~nState: ~p~n", [Req, State]),
    Method = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req),

    case {Method, HasBody} of
        {<<"GET">>, false} ->
            handle_get(Req);
        {<<"POST">>, true} ->
            handle_post(Req);
        {<<"DELETE">>, false} ->
            handele_delete(Req);
        {_, _} ->
            ok
        end,
    {ok, Req, State}.

%% @doc function handeling GET /abonent/<NUM> and GET /abonents requests
handle_get(Req) ->
    Path = binary:split(cowboy_req:path(Req), <<"/">>, [global, trim_all]),
    io:format("Path: ~p~n", [Path]),

    case Path of
        [<<"abonents">>] ->
            Data = web_rtp_db:read_all(),
            ConvertedData = lists:map(fun({Table, Num, Name}) ->
                #{
                    table => Table, 
                    num => Num, 
                    name => erlang:list_to_binary(Name),
                    msg => erlang:list_to_binary(element(2, call_abonent(erlang:integer_to_list(Num))))
                }
            end, Data),
            Response = jsone:encode(#{response => ConvertedData}),
            DeliverRes= cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req);
        [<<"abonent">>, Number ] ->
            Data = web_rtp_db:read(erlang:binary_to_integer(Number)),
            {ok, RespMsg} = call_abonent(erlang:binary_to_list(Number)),
            ConvertedData = lists:map(fun({Table, Num, Name}) ->
                #{
                    table => Table, 
                    num => Num, 
                    name => erlang:list_to_binary(Name),
                    msg => erlang:list_to_binary(RespMsg)
                }
            end, Data),
            Response = jsone:encode(#{response => ConvertedData}),
            DeliverRes= cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req);
        _ ->
            DeliverRes = cowboy_req:reply(404, #{}, <<"Oops! Requested page not found.">>, Req)
    end,

    io:format("Reuqest was handelled!~n"),
    {ok, DeliverRes}.

%% @doc function handeling POST /abonent/ {BODY} requests
handle_post(Req) ->
    Path = binary:split(cowboy_req:path(Req), <<"/">>, [global, trim_all]),
    {ok, DataBin, _Req0} = cowboy_req:read_body(Req),
    Body = jsone:decode(DataBin),

    io:format("Path: ~p~n", [Path]),
    io:format("DataBin: ~p~n", [DataBin]),
    io:format("Body: ~p~n", [Body]),

    case Path of
        [<<"abonent">>] ->
            {ok, Num} = maps:find(<<"num">>, Body),
            {ok, Name} = maps:find(<<"name">>, Body),
             
            case {is_integer(Num), is_binary(Name)} of
                {true, true} -> 
                    web_rtp_db:insert(Num, binary_to_list(Name)),
                    DeliverRes= cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"Succesfully insert into Database!\nReuqest was handelled!">>, Req);
                _ -> 
                    DeliverRes= cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"Wrong Data format, record wasn't insert into Database!\nReuqest was handelled!">>, Req)                    
            end;
        _ ->
            DeliverRes = cowboy_req:reply(404, #{}, <<"Oops! Requested page not found.">>, Req)
    end, 
    {ok, DeliverRes}.

%% @doc function handeling DELETE /abonent/<NUM> requests
handele_delete(Req) ->
    Path = binary:split(cowboy_req:path(Req), <<"/">>, [global, trim_all]),
    io:format("Path: ~p~n", [Path]),

    case Path of
        [<<"abonent">>, Number ] ->
            web_rtp_db:delete(binary_to_integer(Number)),
            DeliverRes= cowboy_req:reply(201, #{<<"content-type">> => <<"text/plain">>}, <<"Successfully deleted from Database!\nReuqest was handelled!">>, Req);
        _ ->
            DeliverRes = cowboy_req:reply(404, #{}, <<"Oops! Requested page not found.">>, Req)
    end,

    io:format("Reuqest was handelled!~n"),
    {ok, DeliverRes}.


%% @doc function calling abonent using SIP and SDP with oRTP
-spec call_abonent(AbonentId :: string()) ->
     {ok, string()}.
call_abonent(AbonentId) ->  
    From_Uri = "sip:102@test.domain",
    SrvId = test_ip_102,
    case AbonentId of 
        "101" ->
            nksip_app:put(sync_call_time, 30000),
            nksip_config:set_config();
        _ ->
            ok
    end, 

    io:format("Abonent ID: ~p~n SrvId: ~p~nFrom: ~p~n", [AbonentId, SrvId, From_Uri]),

    case whereis(SrvId) of
        _Pid ->
            nksip_uac:register(SrvId, "sip:10.0.20.11", [{sip_pass, "1234"}, contact, {meta, ["contact"]}]);
        undefined ->
            nksip:start_link(SrvId, #{sip_from => From_Uri,plugins => [nksip_uac_auto_auth], sip_listen => "<sip:all:5060;transport=udp>"}),
            nksip_uac:register(SrvId, "sip:10.0.20.11", [{sip_pass, "1234"}, contact, {meta, ["contact"]}])
    end,

    Uri = "sip:" ++ AbonentId ++ "@test.domain",
    PBX_IP = "10.0.20.11",
    SDP = #sdp{address = {<<"IN">>, <<"IP4">>, erlang:list_to_binary(PBX_IP)},
        connect = {<<"IN">>, <<"IP4">>, erlang:list_to_binary(PBX_IP)},
        time = [{0, 0, []}],
        medias = [#sdp_m{media = <<"audio">>,
        port = 9990,
        proto = <<"RTP/AVP">>,
        fmt = [<<"0">>, <<"101">>],
        attributes = [{<<"sendrecv">>, []}]
        }
        ]
    },

    InviteOptions = [
        {add, "x-nk-op", ok},
        auto_2xx_ack,
        get_request,
        {route, "<sip:10.0.20.11;lr>"}, 
        {sip_pass, "1234"},
        {body, SDP}
    ], 

    case nksip_uac:invite(SrvId, Uri, InviteOptions) of
        {ok,200,[{dialog, DialogId}]} -> 
            io:format("Dialog ID:~p~n", [DialogId]),

            {ok, Meta} = nksip_dialog:get_meta(invite_remote_sdp, DialogId),
            io:format("Request Meta:~p~n~n", [Meta]),

            [MediaList | _] = element(18, Meta),
            Port = erlang:element(3, MediaList),
            Remote_PBX_IP =  erlang:binary_to_list(element(3, element(8, MediaList))),
            io:format("Port: ~p~nIp: ~p~n~n", [Port, PBX_IP]),
            
            CurrentDir = "cd apps/web_rtp",
            ConvertVoice = "ffmpeg -i priv/voice/generate.wav -codec:a pcm_mulaw -ar 8000 -ac 1 priv/voice/output.wav -y",
            StartVoice = "./voice_client priv/voice/output.wav " ++ Remote_PBX_IP ++ " " ++ erlang:integer_to_list(Port),
            Cmd = CurrentDir ++ " && " ++ ConvertVoice ++ " && " ++ StartVoice,
            Res = os:cmd(Cmd),
            
            io:format("Cmd ~p~nResult ~p~n", [Cmd, Res]),
        
            nksip_uac:bye(DialogId, []),

            Response = "Dialog was started with code 200.\nDialog was finished succesfully!\n",
            io:format("Response: ~p~n", [Response]);
        {ok,480,_}->
            Response = "Code 480. Dialog wasn't started.\nAbonent " ++ AbonentId ++ " is could not respond now!\n",
            io:format("Response: ~p~n", [Response]);
        {ok,486,_}->
            Response = "Code 486. Dialog wasn't started.\nAbonent " ++ AbonentId ++ " is Busy Here\n",
            io:format("Response: ~p~n", [Response]);    
        {ok,404,_} ->
            Response = "Code 404. Error! Dialog wasn't started.\nAbonent " ++ AbonentId ++ " is NOT FOUND!\n",
            io:format("Response: ~p~n", [Response]);
        {ok, Code, _} -> 
            Response = "Response code " ++ erlang:integer_to_list(Code) ++ "!\nAn error occured!\n",
            io:format("Response: ~p~n", [Response]);
        _ -> 
            Response = "Error! An unhandled error occurring during invite request!\n",
            io:format("Response: ~p~n", [Response])
    end,
    {ok, Response}.