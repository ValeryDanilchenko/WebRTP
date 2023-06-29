-module(web_rtp_db).
-author("ValeryDanilchenko").

-behaviour(gen_server).

%%%%%%%%%%%%%% API %%%%%%%%%%%%%%
-export([start/0, stop/0]).
-export([insert/2, read/1, delete/1, read_all/0]).

%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(abonent, {
    num     :: non_neg_integer(),
    name    :: string()
}).

%%%%%%%%%%%%%% API %%%%%%%%%%%%%%
%% @doc API function for starting web_rtp_db module
-spec start() -> 
    {ok, pid()}.
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc API function to exit with 'normal' reason and close and clean database
-spec stop() -> 
    ok.
stop()->
    gen_server:stop(?SERVER).

%% @doc async API function adding new record into database
-spec insert(Num :: non_neg_integer(), AbonentName :: string()) -> 
    ok.
insert(Num, AbonentName) ->
    gen_server:cast(?MODULE, {insert, Num, AbonentName}).

%% @doc API function reads record with key = 'Num'
-spec read(Num :: non_neg_integer()) -> 
    {abonent, Num :: non_neg_integer(), AbonentName :: string()}.
read(Num) ->
    gen_server:call(?MODULE, {read, Num}).

%% @doc API function reads all records from database
-spec read_all() -> 
    list({abonent, Num :: non_neg_integer(), AbonentName :: string()}).
read_all()->
    gen_server:call(?MODULE, {read_all}).

%% @doc async API function deletes record with key = 'Num' from database
-spec delete(Num :: non_neg_integer()) -> 
    ok.
delete(Num) ->
    gen_server:cast(?MODULE, {delete, Num}).


%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%
init(_Args =State) ->
    io:format("~p init callback, was called!~n", [?MODULE]),
    case create_table() of
        {atomic, ok} ->
            io:format("Table 'abonent' was created!~n");
        {aborted, {already_exists, abonent}} ->
            io:format("Table 'abonent' already exists!~n")
    end,
    {ok, State}.


create_table() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(abonent, [ 
            {disc_copies, [node()]}, {attributes, record_info(fields, abonent)}]).
    % mnesia:create_schema([web_rtp]),
    % mnesia:start(),
    % mnesia:create_table(abonent, [ 
    %         {disc_copies, [web_rtp]}, {attributes, record_info(fields, abonent)}]).



handle_call({read, Num}, _From, State) ->
    Fun = 
        fun() ->
            mnesia:read(abonent, Num)
        end,
    {atomic, Result} = mnesia:transaction(Fun),
    {reply, Result, State};
handle_call({read_all}, _From, State) ->
    Fun = 
        fun() ->
            mnesia:select(abonent, [{'_', [], ['$_']}])
        end,
    {atomic, Result} = mnesia:transaction(Fun),
    {reply, Result, State}.

handle_cast({insert, Num, Name},  State) ->
    Rec = #abonent{num = Num, name = Name},
    Fun = 
        fun() ->
            case mnesia:read(abonent, Num) of
                [] ->
                    mnesia:write(Rec);
                [_] ->
                    {error, already_exists}
            end
        end,
    Result = mnesia:transaction(Fun),
    io:format("Record was insert with code: ~p~n", [Result]),
    {noreply, State};
handle_cast({delete, Num},  State) ->
    Fun = 
        fun() ->
            case mnesia:read(abonent, Num) of
                [_] ->
                    mnesia:delete({abonent, Num});
                [] ->
                    {error, wrong_number}
            end            
        end,
    Result = mnesia:transaction(Fun),
    io:format("Record was deleted with code: ~p~n", [Result]),
    {noreply, State};
handle_cast(Info, State) ->
    io:format("Received async message ~p~n", [Info]),
    {noreply, State}.


handle_info(Msg, State) ->
    io:format("Received message ~p~n", [Msg]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("Database was terminated with reason: ~p~n", [Reason]),
    case Reason of
        normal ->
            mnesia:delete_table(abonent);
        _ ->
            ok
    end.