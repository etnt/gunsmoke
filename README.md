# gunsmoke = gun + cowboy + tls + websocket
> An example usage

`gunsmoke` is an example of how to make use of `gun`,
`cowboy`, `TLS` and `Websocket`.

It can run over either TCP or TLS.

For our TLS server we will use a self-signed cert that we
can create like this:

```shell
$ cd cert
$ make
```

You will be prompted for some info that is used in the
making of the certs and keys.

Then you need to update the location of your cert files
in the `src/gunsmoke.app.src` file:

```erlang
%% Change the paths accordingly (wherever used)!    
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

First we can run the example just using TCP.

In the Server shell, start a listener as:

```erlang
2> gunsmoke_server:start_tcp().
%% Check that it actually is listening to port: 8888
3> inet:i().
```

In the Client shell, connect to the server as:

```erlang
2> gunsmoke_client:start_tcp().

%% If it works, you will see something like:
gunsmoke_client79: client config: #{client_tls_opts =>
                                     [{verify,verify_peer},
                                      {log_level,error},
                                      {versions,['tlsv1.2']},
                                      {cacertfile,
                                       "/home/tobbe/git/gunsmoke/cert/ca.crt"}],
                                    disable_hostname_check => true,
                                    port => 8888,
                                    server_ip => {127,0,0,1},
                                    tls_port => 9999,use_tls => false}
gunsmoke_client108: connecting via TCP: IP={127,0,0,1} , Port=8888
{ok,<0.313.0>}
gunsmoke_client191: client got: gun_upgrade , sending Hello!
gunsmoke_client196: handle_info, client got: {gun_ws,<0.314.0>,
                                              #Ref<0.318723597.4279500801.102590>,
                                              {text,
                                               <<"Welcome to the gunsmoke server!">>}}
gunsmoke_client196: handle_info, client got: {gun_ws,<0.314.0>,
                                              #Ref<0.318723597.4279500801.102590>,
                                              {text,<<"Hello from CLient!">>}}
```


On the Server side you will also see some printouts.

Now, let's run our example over TLS.

In the Server shell, start a listener as:

```erlang
2> gunsmoke_server:start_tls().
%% Check that it actually is listening to port: 9999
3> inet:i().
```
In the Client shell, connect to the TLS server as:


```erlang
gunsmoke_client79: client config: #{client_tls_opts =>
                                     [{verify,verify_peer},
                                      {log_level,error},
                                      {versions,['tlsv1.2']},
                                      {cacertfile,
                                       "/home/tobbe/git/gunsmoke/cert/ca.crt"}],
                                    disable_hostname_check => true,
                                    port => 8888,
                                    server_ip => {127,0,0,1},
                                    tls_port => 9999,use_tls => true}
gunsmoke_client100: connecting via TLS: IP={127,0,0,1} , Port=9999
gunsmoke_client191: client got: gun_upgrade , sending Hello!
gunsmoke_client196: handle_info, client got: {gun_ws,<0.306.0>,
                                              #Ref<0.318723597.4279500801.102546>,
                                              {text,
                                               <<"Welcome to the gunsmoke server!">>}}
gunsmoke_client196: handle_info, client got: {gun_ws,<0.306.0>,
                                              #Ref<0.318723597.4279500801.102546>,
                                              {text,<<"Hello from CLient!">>}}
```

Very nice and we are done.

__Goodbye!__

