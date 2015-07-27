PROJECT = e_chat_server
ERLC_OPTS= "+{parse_transform, lager_transform}"

DEPS = cowboy jsx lager pgapp

dep_cowboy = https://github.com/ninenines/cowboy.git 1.0.1
dep_jsx = https://github.com/talentdeficit/jsx.git
dep_lager = https://github.com/basho/lager.git 2.1.1
dep_pgapp = https://github.com/epgsql/pgapp.git

include erlang.mk
