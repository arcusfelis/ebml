-module(ebml_integer).
-export([encode/1, decode/1]).
-compile([export_all]).

%%% Variable size integer
 
%%% For both element ID and size descriptor EBML uses a variable size
%%% integer, coded according to a schema similar to that of UTF-8
%%% [UTF-8] encoding. The variable size integer begins with zero or
%%% more zero bits to define the width of the integer. Zero zeroes
%%% means a width of one byte, one zero a width of two bytes etc. The
%%% zeroes are followed by a marker of one set bit and then follows the
%%% actual integer data. The integer data consists of alignment data
%%% and tail data. The alignment data together with the width
%%% descriptor and the marker makes up one ore more complete bytes. The
%%% tail data is as many bytes as there were zeroes in the width
%%% descriptor, i.e. width-1.
 
%%%   VINT           = VINT_WIDTH VINT_MARKER VINT_DATA
%%%   VINT_WIDTH     = *%b0
%%%   VINT_MARKER    = %b1
%%%   VINT_DATA      = VINT_ALIGNMENT VINT_TAIL
%%%   VINT_ALIGNMENT = *BIT
%%%   VINT_TAIL      = *BYTE
 
%%% An alternate way of expressing this is the following definition,
%%% where the width is the number of levels of expansion.
 
%%%   VINT = ( %b0 VINT 7BIT ) / ( %b1 7BIT )
 
%%% Some examples of the encoding of integers of width 1 to 4. The x:es
%%% represent bits where the actual integer value would be stored.
 
%%% Width  Size  Representation
%%%   1    2^7   1xxx xxxx
%%%   2    2^14  01xx xxxx  xxxx xxxx
%%%   3    2^21  001x xxxx  xxxx xxxx  xxxx xxxx
%%%   4    2^28  0001 xxxx  xxxx xxxx  xxxx xxxx  xxxx xxxx

%% @doc Read integer from binary.
%% Returns {integer(), Tail}.
-spec decode(binary()) -> {integer(), binary()}.
decode(<<1:1, X:7,  T/binary>>) -> {X, T};
decode(<<1:2, X:14, T/binary>>) -> {X, T};
decode(<<1:3, X:21, T/binary>>) -> {X, T};
decode(<<1:4, X:28, T/binary>>) -> {X, T};

decode(<<0:4, _>> = B) -> decode_long(B, 5, 35).


decode_long(B, W, S) ->
	case B of
	<<1:W, X:S, T/binary>> -> 
		{X, T};
	<<0:W, _>> ->
		decode_long(B, W+1, S+7)
	end.



encode(X) when is_integer(X) -> 
	W = width_of_integer(X),
	S = W*7,
	<<1:W, X:S>>.


width_of_integer(0) ->
	1;
width_of_integer(X) ->
	width_of_integer(X, 0).


width_of_integer(0, W) ->
	W;
width_of_integer(X, W) ->
	width_of_integer(X bsr 7, W+1).
	

	
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(M, ?MODULE).

encode_test_() ->
	E = fun ?M:encode/1,
	D = fun ?M:decode/1,
    [?_assertEqual(D(E(100)), {100, <<>>})
	,?_assertEqual(D(E(666)), {666, <<>>})
	,?_assertEqual(D(E(0)), {0, <<>>})
	,?_assertEqual(D(E(1)), {1, <<>>})
	].

-endif.
