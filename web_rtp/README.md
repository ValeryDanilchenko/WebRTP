WebRtp
=====
![Erlang/OTP](https://img.shields.io/badge/Erlang/OTP-24-green)
![Alpine](https://img.shields.io/badge/Alpine-3.17-red)
![NKSIP](https://img.shields.io/badge/NKSIP-0.6.1-blue)
![COWBOY](https://img.shields.io/badge/COWBOY-2.9.0-yellow)
![JSONE](https://img.shields.io/badge/JSONE-1.8.0-green)



## Курсовая работа
-----

WEB RTP осуществляет обзовн абонентов, подключенных к виртуальной АТС

Успешно реализованны требования:
- Все абоненты хранятся в БД Mnesia
- присутствует REST API для осуществления обзвона всех абонентов или одного определенного по номеру телефона, а так же для добавления или удаления абонетов из БД, соответственно
```HTTP
HTTP GET/call/abonents
HTTP GET/call/abonent/{abonent_number}
HTTP POST/call/abonent/{"num":abonent_number, "name":"abonent_name"}
HTTP DELETE/call/abonent/{abonent_number}
```
- обзвон реализоваy среди абонентов, подключенных к виртуальной АТС ECSS10 с применением протоколов SIP и SDP,
и использованием библиотеки nk_sip
- проект разработан на языке Erlang и собран с помощью Rebar3, в проекте так же содержатся файлы для работы с RTP на языке C применением библиотеки oRTP
- успешно осуществлана передача голосовых пакетов в клиент Zoiper5
- реализован Docker контейнер


[Демо видео](https://youtu.be/zl5v_5luRfo)



Установка
====
Клонируем репозиторий 

    $ git clone https/

Далее переходим в рабочую директори, компилируем и запускаем приложение с помощью rebar 3 
    $ cd web_rtp
    
    $ rebar3 compile
    $ rebar3 shell

Переходим на http://localhost:8080 для отправки запросов к WebRTP


Запросы
====

- ## HTTP GET/call/abonent/{abonent_number}
    Запрос вызывает абонента с номером abonent_number с помощью протокола SIP 
    
    ![SIP Scheme](/web_rtp/apps/web_rtp/data/media/SIP_scheme.png)

    ### Пример выполнеиния вызова:

    ![SIP Scheme](/web_rtp/apps/web_rtp/data/media/call_abonent_101.gif)

    Вызывется функция web_rtp_nadler:call_abonent/1

    ```erlang
    call_abonent(AbonentId) ->  
        % определяем абоента, от которого будет исходить вызов 
        From_Uri = "sip:102@test.domain",
        SrvId = test_ip_102,
        
        %увеличиваем время на звонок для зарегистрированного баонента 101
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

        % задаем параметры необходимые для вызова заданного абонента
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

        % вызов абонента
        case nksip_uac:invite(SrvId, Uri, InviteOptions) of
            % если вызова принят вызываемым абонентом
            {ok,200,[{dialog, DialogId}]} -> 
                io:format("Dialog ID:~p~n", [DialogId]),

                %получаем метаданные из диалога
                {ok, Meta} = nksip_dialog:get_meta(invite_remote_sdp, DialogId),
                io:format("Request Meta:~p~n~n", [Meta]),
                
                % получаем Порт и IP абонета, куда необходимо передать звук 
                [MediaList | _] = element(18, Meta),
                Port = erlang:element(3, MediaList),
                Remote_PBX_IP =  erlang:binary_to_list(element(3, element(8, MediaList))),
                io:format("Port: ~p~nIp: ~p~n~n", [Port, PBX_IP]),
                
                %отправляем голосовые пакеты абоненту
                CurrentDir = "cd apps/web_rtp",
                ConvertVoice = "ffmpeg -i priv/voice/generate.wav -codec:a pcm_mulaw -ar 8000 -ac 1 priv/voice/output.wav -y",
                StartVoice = "./voice_client priv/voice/output.wav " ++ Remote_PBX_IP ++ " " ++ erlang:integer_to_list(Port),
                Cmd = CurrentDir ++ " && " ++ ConvertVoice ++ " && " ++ StartVoice,
                Res = os:cmd(Cmd),
                
                io:format("Cmd ~p~nResult ~p~n", [Cmd, Res]),

                %завершаем диалог
                nksip_uac:bye(DialogId, []),

                Response = "Dialog was started with code 200.\nDialog was finished succesfully!\n",
                io:format("Response: ~p~n", [Response]);
            % если до абонента удалось дозвониться, но он не может принять звонок
            {ok,480,_}->
                Response = "Code 480. Dialog wasn't started.\nAbonent " ++ AbonentId ++ " is could not respond now!\n",
                io:format("Response: ~p~n", [Response]);
            % если абонент уже находится в диалоге
            {ok,486,_}->
                Response = "Code 486. Dialog wasn't started.\nAbonent " ++ AbonentId ++ " is Busy Here\n",
                io:format("Response: ~p~n", [Response]); 
            % если абонент не зарегестрирован на сервере   
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
    ```


- ## HTTP GET/call/abonents
Аналогично с call/abonent/{number} обрабатывается запрос на вызов абонентов. Для каждого абонента из БД Mnesia вызывается функция web_rtp_nadler:call_abonent/1

![Call abonents](/web_rtp/apps/web_rtp/data/media/call_abonents.png)


- ## HTTP POST/call/abonent
Запрос для добавления нового абонета в таблицу 

![POST request](/web_rtp/apps/web_rtp/data/media/post_req.png)


- ## HTTP DELETE/call/abonent/{number}
Запрос для удаления абонета из таблицы

![DELETE request](/web_rtp/apps/web_rtp/data/media/delete_req.png)



Docker
===
Для домонстрации проекта будет доступен Docker образ:
....


Запустить и проверить работу в ближайшее время возможно будет:

    $ docker run valerydanilchenko-web-rtp:latest
