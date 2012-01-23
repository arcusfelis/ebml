
-module(ebml_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(ebml).


start(_Type, _Args) ->
	ebml_sup:start_link().

stop(_State) ->
	ok.

