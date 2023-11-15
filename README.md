# gunsmoke = gun + cowboy + tls + websocket
> An error reproduction

`gunsmoke` could be an example of how to make use of `gun`,
`cowboy`, `TLS` and `Websocket`; unfortunately it does not
work at the moment, so this is a reproduction of the error.

For our TLS server we will use a self-signed cert that we
can create like this:

``` 4d
$ cd cert
$ make
```

You will be prompted for some info that is used in the
making of the certs and keys.

Then you need to update the location of your cert files
in the `src/gunsmoke.app.src` file:

```erlang
%% Change the paths accordingly    
, {cacertfile, "/home/tobbe/git/gunsmoke/cert/ca.crt"}
, {certfile, "/home/tobbe/git/gunsmoke/cert/server.pem"}
, {keyfile, "/home/tobbe/git/gunsmoke/cert/server.key"}
```

We are now ready to run the example.
Use two shells, one from where we will run the Client and
another where we will run the Server.

Start the Server as:

```
$ rebar3 shell --sname server --apps ssl,ranch,cowboy
1> application:load(gunsmoke).
```

Start the Client as:

```
$ rebar3 shell --sname client --apps ssl,gun
1> application:load(gunsmoke).
```

First we can run the example just using TCP to check that Websockets
actually works.

In the Server shell, start a listener as:

```erlang
2> gunsmoke_server:start_tcp().
%% Check that it actually is listening
3> inet:i().
```

In the Client shell, connect to the server as:

```erlang
2> gunsmoke_client:start_tcp().

%% If it works, you will see something like: 
gunsmoke_client107: connecting via TCP: IP={127,0,0,1} , Port=8888                                   
gunsmoke_client164: client got: gun_upgrade , sending Hello!                                         
gunsmoke_client169: handle_info, client got: {gun_ws,<0.306.0>,
                                              #Ref<0.1143879020.3362521093.153723>,
                                              {text,
                                               <<"Welcome to the gunsmoke server!">>}}
```

On the Server side you will also see some printouts.

Now, let's reproduce the problem, first start a TLS listener
on the Server side:

```erlang
2> gunsmoke_server:start_tls().
%% Check that it actually is listening
3> inet:i().
```
In the Client shell, connect to the TLS server as:

```erlang
2> gunsmoke_client:start_tls().

%% You will see lots of debug output, ending with:
...
gunsmoke_client169: handle_info, client got: {gun_error,<0.308.0>,
                                              #Ref<0.3146592152.1217134599.144411>,
                                              {stream_error,protocol_error,
                                               'Stream reset by server.'}}
```

What is causing this?
