-module(ebml_dtd).

#acc{
	current_name,
	
}.

parse(_Data) ->
	parse('dtd', Data, #acc{}).

% LCOMMENT = "//" *BYTE (CR / LF) ; *BYTE is string without CR/LF
parse(S, "//" ++ T1, Acc1) ->
	{_C, T2} = read_lcomment(T1),
	parse(S, T2, Acc1);

% BCOMMENT = "/*" *BYTE "*/" ; *BYTE is string without "*/"
parse(S, "/*" ++ T1, Acc1) ->
	{_C, T2} = read_bcomment(T1),
	parse(S, T2, Acc1);

parse(S, " " ++ T1, Acc1) ->
	parse(S, T1, Acc1);

parse('dtd', "declare" ++ T1, Acc1) ->
	parse('declare', T1, Acc1);

parse('dtd', "define" ++ T1, Acc1) ->
	parse('define', T1, Acc1);

parse('declare', "elements" ++ T1, Acc1) ->
	parse('header', T1, Acc1);

parse('define', "elements" ++ T1, Acc1) ->
	parse('elements', T1, Acc1);

parse('define', "types" ++ T1, Acc1) ->
	parse('types', T1, Acc1);

% STATEMENT = NAME S ":=" S DEFS S ";"
parse('header', T1, Acc1) ->
	{Name, T2} = read_name(T1),
	parse('header_name', T2, Acc1#acc{ 
			current_name=Name, 
			current_value=undefined });



read_lcomment(T) ->
	read_lcomment(T, "").


read_lcomment("\r\n" ++ T, Acc) ->
	{Acc, T};

read_lcomment([H|T], Acc) ->
	read_bytes([H|T], Acc);

read_comment([], Acc) ->
	{lists:reverse(Acc), []}.


read_bcomment(T) ->
	read_bcomment(T, "").


read_bcomment("*/" ++ T, Acc) ->
	{Acc, T};

read_lcomment([H|T], Acc) ->
	read_bytes([H|T], Acc).


read_name(T) ->
	read_name(T, "").


read_name([H|T], Acc) 
	when H >= $a, H =< $z;
		 H >= $A, H =< $Z;
		 H =:= $_, Acc =/= [] ->
	read_name(T, [H|Acc]);

read_name(T, Acc) ->
	{lists:reverse(Acc), T}.
	


read_integer_value([$-|T]) ->
	{Int, Acc} = read_uinteger_value(T),
	{-Int, Acc};

read_integer_value([$+|T]) ->
	read_uinteger_value(T);

read_integer_value(T) ->
	read_uinteger_value(T).
	

read_uinteger_value([H|T], Acc) 
	when H >= $0, H =< 9 ->
	read_integer_value(T, Acc*10 + H);

read_uinteger_value(T, Acc) ->
	{Acc, T}.
	


% Example: T1="-1.23e+4"
read_number(T1) ->
	% Int = -1, T2 = ".23e+4"
	{Int, T2} = read_integer_value(T1),
	case T2 of
	[$.|T3] ->
		% T3 = "23e+4", T4="e+4"
		{Frac1, T4} = read_integer_value(T3),
		% Frac1=23, Frac2=0.23
		Frac2 = after_paint(Frac1),
		% Num1 = -1.23
		Num1 = case Int < 0 of
			true  -> Int - Frac2;
			false -> Int + Frac2
			end,

		case T4 of
		[$e|T5] -> 
			% Exp=4
			{Exp, T6} = read_integer_value(T5),
			Num2 = Num1 * math:pow(10, -Exp),
			{Num2, T6};

		_ -> {Num1, T4}
		end;
			
	_ ->
		{Int, T2}
	end.
	

%% X = 345, Result = 0.345
after_paint(X) when X>0, X=<1 ->
	afret_paint(1/10);

after_paint(X) when X>0 ->
	X.


	
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(M, ?MODULE).

read_integer_value_test_() ->
	F = fun read_integer_value/1,
    [?_assertEqual(F("100"),      {100, ""})
    ,?_assertEqual(F("102 test"), {102, " test"})
    ,?_assertEqual(F("-65"),      {-65, ""})
	].

read_number_test_() ->
	F = fun read_number/1,
    [?_assertEqual(F("100"),      {100, ""})
    ,?_assertEqual(F("102 test"), {102, " test"})
    ,?_assertEqual(F("-65"),      {-65, ""})
    ,?_assertEqual(F("0.1e-1"),   {0.1e-1, ""})
    ,?_assertEqual(F("0.1e3"),    {0.1e3, ""})
    ,?_assertEqual(F("0.1e+3"),   {0.1e3, ""})
    ,?_assertEqual(F("-0.1e-4"),  {-0.1e-4, ""})
	].

-endif.
