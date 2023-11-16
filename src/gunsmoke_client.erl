%%%-------------------------------------------------------------------
%%% @author Torbjorn Tornkvist <kruskakli@gmail.com>
%%% @copyright (C) 2023, Torbjorn Tornkvist
%%% @doc Client, connecting either over TLS or TCP
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gunsmoke_client).

-behaviour(gen_server).


%% gen_server callbacks
-export([ start_tcp/0
        , start_tls/0
        , init/1
        , handle_continue/2
        , handle_call/3
        , handle_info/2
        , handle_cast/2
        , format_status/2
        , terminate/2
        , code_change/3
        ]).

-define(dbg(FmtStr, Args), io:format("~p~p: "++FmtStr,[?MODULE,?LINE|Args])).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_tcp() -> {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()}} |
          {error, Error :: term()} |
          ignore.

start_tcp() ->
    gen_server:start(?MODULE, #{use_tls => false}, []).


-spec start_tls() -> {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()}} |
          {error, Error :: term()} |
          ignore.

start_tls() ->
    gen_server:start(?MODULE, #{use_tls => true}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
          {ok, State :: term(), Timeout :: timeout()} |
          {ok, State :: term(), hibernate} |
          {stop, Reason :: term()} |
          ignore.
init(CfgMap) ->
    process_flag(trap_exit, true),
    {ok, CfgMap, {continue, start_client}}.

handle_continue(start_client, CfgMap) ->
    continue(get_client_config(CfgMap)).

continue(CfgMap) ->
    try
        ?dbg("client config: ~p~n", [CfgMap]),

        {ok, ConnPid} = gun_open(CfgMap),
        {ok, _Protocol} = gun:await_up(ConnPid),

        gun:ws_upgrade(ConnPid, "/", []),

        %% Now, wait for the gun_upgrade message to arrive in handle_info/2.
        {noreply, CfgMap}

    catch
        _Etype:_Err:_Estack ->
            ?dbg("<ERROR> ~p~n",[{_Etype,_Err,_Estack}]),
            {noreply, CfgMap}
    end.


gun_open(#{use_tls := true} = CfgMap) ->
    IP = maps:get(server_ip, CfgMap),
    Port = maps:get(tls_port, CfgMap),
    TLS_opts = maps:get(client_tls_opts, CfgMap),
    ?dbg("connecting via TLS: IP=~p , Port=~p~n",[IP,Port]),
    gun:open(IP, Port, #{ transport => tls
                          %% Note: default protocols are: [http2,http]
                          %% but `http2` will complicate matters here and
                          %% is not really needed for our purposes; we just
                          %% want to bring up the websocket as quickly
                          %% as possible.
                        , protocols => [http]
                        , tls_opts => TLS_opts
                        });
gun_open(#{use_tls := false} = CfgMap) ->
    IP = maps:get(server_ip, CfgMap),
    Port = maps:get(port, CfgMap),
    ?dbg("connecting via TCP: IP=~p , Port=~p~n",[IP,Port]),
    gun:open(IP, Port).



get_client_config(CfgMap) ->
    gunsmoke_app:get_config(client_config(), CfgMap).

client_config() ->
    [server_ip, port, tls_port, client_tls_opts].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
          {reply, Reply :: term(), NewState :: term()} |
          {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
          {reply, Reply :: term(), NewState :: term(), hibernate} |
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
          {stop, Reason :: term(), NewState :: term()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: normal | term(), NewState :: term()}.

handle_info({gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers},
            State) ->
    ?dbg("client got: gun_upgrade , sending Hello!~n",[]),
    gun:ws_send(ConnPid, StreamRef, {text, "Hello from CLient!"}),
    {noreply, State};
%%
handle_info(_Info, State) ->
    ?dbg("handle_info, client got: ~p~n",[_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
          {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
