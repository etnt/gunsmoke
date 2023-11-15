%%%-------------------------------------------------------------------
%%% @copyright (C) 2023, Torbjorn Tornkvist
%%% @doc Server, listening to either TLS or TCP
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gunsmoke_server).

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

%% cowboy callbacks
-export([ init/2
        , websocket_init/1
        , websocket_handle/2
        , websocket_info/2
        ]).

-define(dbg(FmtStr, Args), io:format("~p~p: "++FmtStr,[?MODULE,?LINE|Args])).

-define(SERVER_TCP, gunsmoke_tcp_server).
-define(SERVER_TLS, gunsmoke_tls_server).


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
    gen_server:start_link({local, ?SERVER_TCP}, ?MODULE, #{use_tls => false}, []).

start_tls() ->
    gen_server:start_link({local, ?SERVER_TLS}, ?MODULE, #{use_tls => true}, []).

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
    {ok, CfgMap, {continue, start_server}}.

handle_continue(start_server, CfgMap) ->
    continue(get_server_config(CfgMap)).

continue(CfgMap) ->
    try
        Dispatch =
            cowboy_router:compile(
              [
               %% {HostMatch, list({PathMatch, Handler, InitialState})}
               {'_', [{"/", ?MODULE, CfgMap#{state => init}}]}
              ]),

        {ok,_} = start_listener(CfgMap, Dispatch),

        {noreply, CfgMap}

    catch
        _Etype:_Err:_Estack ->
            %% FIXME log error
            ?dbg("<ERROR> ~p~n", [{_Etype,_Err,_Estack}]),
            {noreply, CfgMap}
    end.


start_listener(#{use_tls := true} = CfgMap, Dispatch) ->
    IP = maps:get(listen_ip, CfgMap),
    Port = maps:get(tls_port, CfgMap),
    TLS_opts = maps:get(server_tls_opts, CfgMap),

    ?dbg("starting HTTPS listener on Port: ~p~n",[Port]),
    cowboy:start_tls(
      https_listener,
      [ {ip, IP}
      , {port, Port}
      ] ++ TLS_opts,
      #{env => #{dispatch => Dispatch}}
     );
%%
start_listener(CfgMap, Dispatch) ->
    IP = maps:get(listen_ip, CfgMap),
    Port = maps:get(port, CfgMap),

    ?dbg("starting HTTP listener on Port: ~p~n",[Port]),
    cowboy:start_clear(
      http_listener,
      [ {ip, IP}
      , {port, Port}
      ],
      #{env => #{dispatch => Dispatch}}
     ).



get_server_config(CfgMap) ->
    gunsmoke_app:get_config(server_config(), CfgMap).

server_config() ->
    [listen_ip, port, tls_port, server_tls_opts].

%%
%% Cowboy callback
%%
init(Req0, StateMap) ->
    ?dbg("server got connected, upgrading to Websocket!~n", []),
    {cowboy_websocket,
     Req0,
     StateMap}.

%%
%% Cowboy Websocket callbacks
%%
websocket_init(Map) ->
    ?dbg("server websocket init, sending Welcome!~n",[]),
    {[{text,"Welcome to the gunsmoke server!"}], Map}.


websocket_handle(Frame, State) ->
    ?dbg("server got Frame, echoing back: ~p~n",[Frame]),
    {Frame, State}.


websocket_info(_Info, State) ->
    ?dbg("server got unknown info: ~p~n",[_Info]),
    {ok, State}.




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
handle_info(_Info, State) ->
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
