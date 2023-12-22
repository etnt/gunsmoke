.PHONY: all compile xref clean server client

all: CA compile xref

server:
	rebar3 shell --sname server --apps ssl,ranch,cowboy --eval 'application:load(gunsmoke).'

client:
	rebar3 shell --sname client --apps ssl,gun --eval 'application:load(gunsmoke).'

compile:
	rebar3 compile

xref:
	rebar3 xref

CA:
	git clone --depth 1 https://github.com/etnt/myca.git CA

clean:
	rebar3 clean
