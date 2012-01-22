-module(ebml_dtd).
-export([parse/1]).
-compile([export_all]).

-record(statement, {
	name :: name(),
	value :: defs()
}).

-record(dtype, {
	name :: name(),
	type :: types(),
	properties :: properties()
}).

-record(delement, {
	name :: name(),
	id :: integer(),
	type :: types(),
	properties :: properties(),
	elements = []
}).

-type statement() :: #statement{}.
-type dtype() :: #dtype{}.
-type delement() :: #delement{}.

-record(hblock, {
	headers :: [statement()]
}).

-record(tblock, {
	types :: [dtype()]
}).

-record(eblock, {
	elements :: [delement()]
}).

-type hblock() :: #hblock{}.
-type tblock() :: #tblock{}.
-type eblock() :: #eblock{}.

-record(dtd, {
	hblock :: hblock(),
	tblock :: tblock(),
	eblock :: eblock()
}).

-type name() :: atom().


-record(parent, {
	parents :: [name()]
}).

-record(level, {
	from :: integer(),
	to :: integer()
}).

-record(card, {
	pattern :: '*' | '?' | '1' | '+'
}).

-record(def, {
	value :: defs()
}).

-record(range, {
	range_list :: [range_item()]
}).

-record(ordered, {
	value :: yes | no
}).

-record(size, {
	size_list :: [range_item()]
}).

-type parent() :: #parent{}.
-type level() :: #level{}.
-type card() :: #card{}.
-type def() :: #def{}.
-type range() :: #range{}.
-type size() :: #size{}.
-type ordered() :: #ordered{}.

-record(range_item, {
	check :: function()
}).

-type range_item() :: int_range() | uint_range() | float_range() 
	| string_range() | date_range() | binary_range().

-type int_range() :: #range_item{}.
-type uint_range() :: #range_item{}.
-type float_range() :: #range_item{}.
-type string_range() :: #range_item{}.
-type date_range() :: #range_item{}.
-type binary_range() :: #range_item{}.

-type properties() :: [property()].
-type property() :: parent() | level() | card() | def() | range() | size() 
		| ordered().

-type defs() :: int_def() 
	| uint_def() | float_def() | string_def() 
	| date_def() | binary_def() | name().

-type int_def() :: integer().
-type uint_def() :: integer().
-type float_def() :: float().
-type date_def() :: integer().
-type string_def() :: binary().
-type binary_def() :: binary().

-type types() :: int | uint | float | string | date | binary 
	| container | atom().





parse(Str) ->
	dtd(Str).


run(T1, Funs, Acc1) ->
	case cycle(T1, Funs, Acc1) of
		{cycle, T1, _Acc2} ->
			throw('infinite_cycle');

		{cycle, T2, Acc2} ->
			run(T2, Funs, Acc2);

		{break, T2, Acc2} ->
			{break, T2, Acc2};

		{eof, Acc} -> 
			{eof, Acc}
	end.


cycle([_|_]=T1, [Cur|Funs], Acc) ->
	case Cur(T1) of
		{trim, T2} ->
			cycle(T2, [Cur|Funs], Acc);

		skip ->
			cycle(T1, Funs, Acc);

		{put, T2, Elem} ->
			cycle(T2, Funs, [Elem|Acc]);

		{break, T2} ->
			% exit from the current level
			{break, T2, lists:reverse(Acc)};

		{break, T2, Elem} ->
			% Put the current element and exit from the current level.
			{break, T2, lists:reverse([Elem|Acc])};

		{eof, Elem} ->
			% Put the current element and exit.
			{eof, lists:reverse([Elem|Acc])}
	end;
		
cycle([], _Funs, Acc) ->
	{eof, lists:reverse(Acc)};

cycle(T1, [], Acc) ->
	{cycle, T1, lists:reverse(Acc)}.

		





% LCOMMENT = "//" *BYTE (CR / LF) ; *BYTE is string without CR/LF
clean("//" ++ T1) ->
	{_C, T2} = read_lcomment(T1),
	{trim, T2};

% BCOMMENT = "/*" *BYTE "*/" ; *BYTE is string without "*/"
clean("/*" ++ T1) ->
	{_C, T2} = read_bcomment(T1),
	{trim, T2};

clean([H|T]) 
	when H =:= $  ; 
	     H =:= $\t; 
	     H =:= $\r; 
	     H =:= $\n ->
	{trim, T};

clean(_) -> skip.


%% It is `clean' for direct calls.
strip(T1) ->
	case clean(T1) of
		skip       -> T1;
		{trim, T2} -> strip(T2)
	end.


breaker("}" ++ T1) -> {break, T1};
breaker(T1) -> skip.


%% DTD      = *( S / HBLOCK / TBLOCK / EBLOCK )
dtd(T1) ->
	Funs = [ fun clean/1
		   , fun declare/1
		   , fun define/1
		   ],

	{eof, Acc} = run(T1, Funs, []),
	form_dtd_record(Acc).


form_dtd_record(Acc) ->
	#dtd{
		hblock = lists:keyfind(hblock, 1, Acc),
		tblock = lists:keyfind(tblock, 1, Acc),
		eblock = lists:keyfind(eblock, 1, Acc)
	}.


declare("declare" ++ T1) ->
	T2 = strip(T1),
	hblock(T2);

declare(_) -> skip.
 

define("define" ++ T1) ->
	T2 = strip(T1),
	teblock(T2);

define(_) -> skip.


%% HBLOCK   = "declare" S WSP "header" S "{" *(S / STATEMENT) "}"
hblock("header" ++ T1) ->
	"{" ++ T2 = strip(T1),
	
	Funs = [ fun clean/1
		   , fun breaker/1
		   , fun statement/1
		   ],

	{break, T3, Acc} = run(T2, Funs, []),
	Elem = #hblock{
		headers=Acc	
	},
	{put, T3, Elem}.


% STATEMENT = NAME S ":=" S DEFS S ";"
statement(T1) ->
	{Name, T2} = read_name(T1),
	":=" ++ T3 = strip(T2),
	T4 = strip(T3),
	{Value, T5} = read_value(T4),
	";" ++ T6 = strip(T5),
	Elem = #statement{
		name=Name,
		value=Value
	},
	{put, T6, Elem}.
	

%% @doc tblock & eblock
teblock("elements" ++ T1) ->
	"{" ++ T2 = strip(T1),
	{break, T3, Acc} = elements(T2),
	Elem = #eblock{
		elements=Acc	
	},
	{put, T3, Elem};

%% TBLOCK   = "define" S WSP "types" S "{" *(S / DTYPE) "}"
teblock("types" ++ T1) ->
	"{" ++ T2 = strip(T1),
	
	Funs = [ fun clean/1
		   , fun breaker/1
		   , fun dtype/1
		   ],

	{break, T3, Acc} = run(T2, Funs, []),
	Elem = #tblock{
		types=Acc	
	},
	{put, T3, Elem}.


%% Without `{'.
elements(T1) ->
	
	Funs = [ fun clean/1
		   , fun breaker/1
		   , fun delement/1
		   ],

	run(T1, Funs, []).


%% The simple statements are typically used for value elements and
%% consists of a name followed by ":=", id, type and optionally
%% properties.

%%   VELEMENT = NAME S ":=" S ID WSP S TYPE S (PROPERTIES S *1";")/";"

%% The block version of the element statements are only used to
%% express parent-children relations. See section 3.5.

%%   CELEMENT = NAME S ":=" S ID WSP S "container" S *1PROPERTIES S
%%              ("{" *DELEMENT "}")/";"

%%   DELEMENT = VELEMENT / CELEMENT / "%children;"
delement("%children;" ++ T1) ->
	{put, children, T1};
delement(T1) ->
	{Name, T2} = read_name(T1),
	":=" ++ T3 = strip(T2),
	T4 = strip(T3),
	{Id, T5} = read_id(T4),
	T6 = strip(T5),
	{Type, T7} = read_type(T6),
	{break, T8, Props} = 
		case strip(T7) of
			"[" ++ Ta -> parse_props(Type, Ta);
			Ta -> {break, Ta, []}
		end,


	{break, T10, Childrens} = 
		case T8 of	
			"{" ++ T9 when Type =:= container ->
				elements(T9);
			";" ++ T9 -> 
				{break, T9, []};
			_ ->
				{break, T8, []}
		end,

	Elem = #delement{
		name = Name,
		id = Id,
		type = Type,
		properties = Props,
		elements = Childrens
	},

	{put, T10, Elem}.
	



%% ; TYPE must be defined. PROPERTIES must only use DEF and RANGE.
%% DTYPE    = NAME S ":=" S TYPE S (PROPERTIES S *1";")/";"
%% TYPE     = VTYPE / CTYPE
%% VTYPE    = "int" / "uint" / "float" / "string" / "date" /
%%            "binary" / NAME
%% CTYPE    = "conainer" / NAME
dtype(T1) ->
	{Name, T2} = read_name(T1),
	":=" ++ T3 = strip(T2),
	T4 = strip(T3),
	{Type, T5} = read_type(T4),
	{break, T6, Props} = 
		case strip(T5) of
			";" ++ Ta -> {break, Ta, []};
			"[" ++ Ta -> parse_props(Type, Ta)
		end,
				
	Elem = #dtype{
		name=Name,
		type=Type,
		properties=Props
	},
	{put, T6, Elem}.


props_breaker("]" ++ T1) ->
	case strip(T1) of
		";" ++ T2 ->
			{break, T2};
		T2 ->
			{break, T2}
	end;
props_breaker(T1) -> skip.


parse_props(Type, T1) ->
	Funs = [ fun clean/1
		   , fun props_breaker/1
		   , property(Type)
		   ],

	run(T1, Funs, []).

%% PROPERTY   = PARENT / LEVEL / CARD / DEF / RANGE / SIZE
property(Type) ->
	fun(T1) ->
		{Name, T2} = read_name(T1),
		":" ++ T3 = strip(T2),
		T4 = strip(T3),

		case Name of
			'parent' ->
				parents(T4);
			'level' ->
				level(T4);
			'card' ->
				card(T4);
			'def' ->
				def(T4, Type);
			'range' ->
				range_list(T4, Type);
			'size' ->
				size_list(T4);
			'ordered' ->
				ordered(T4)
		end
	end.


%% PARENT   = "parent" S ":" S PARENTS S ";"
%% PARENTS  = NAME / ( NAME S "," S PARENTS )
parents(T1) ->
	parents(T1, []).

parents(T1, Acc1) ->
	{Name, T2} = read_name(T1),
	Acc2 = [Name|Acc1],

	case strip(T2) of
	"," ++ T3 ->
		T4 = strip(T3),
		parents(T4, Acc2);

	";" ++ T3 ->
		Elem = #parent{
			parents=lists:reverse(Acc2)
		},
		{put, T3, Elem}
	end.
				
	
%% LEVEL    = "level"  S ":" S 1*DIGIT *(".." *DIGIT) S ";"
level(T1) ->
	{From, T2} = read_uinteger_value(T1),
	{To, T4} = case T2 of
		".." ++ T3 ->
			 read_uinteger_value(T3);

		T3 ->
			{undefined, T3}
		end,

	";" ++ T5 = strip(T4),

	Elem = #level{
		from=From,
		to=To
	},
	{put, T5, Elem}.
		

%% CARD     = "card"   S ":" S ( "*" / "?" / "1" / "+" ) S ";"
card([H|T1]) when H =:= $*; H =:= $?; H =:= $1; H =:= $+ ->
	Elem = #card{
		pattern=list_to_atom([H])
	},
	";" ++ T2 = strip(T1),
	{put, T2, Elem}.
	
%% DEF      = "def"    S ":" S DEFS S ";"
def(T1, Type) ->
	{Value, T2} = read_value(T1, Type),
	";" ++ T3 = strip(T2),

	Elem = #def{
		value=Value
	},
	{put, T3, Elem}.


%% ORDERED  = "ordered" S ":" S ( YES / NO ) S ";"	
ordered(T1) ->
	case T1 of
	"yes" ++ T2 -> Value = yes;
	"1"   ++ T2 -> Value = yes;
	"no"  ++ T2 -> Value = no;
	"0"   ++ T2 -> Value = no
	end,

	";" ++ T3 = strip(T2),
	Elem = #ordered{
		value=Value
	},
	{put, T3, Elem}.


%% RANGE    = "range" S ":" S RANGE_LIST S ";"
%% RANGE_LIST = RANGE_ITEM / ( RANGE_ITEM S "," S RANGE_LIST )
%% RANGE_ITEM = INT_RANGE / UINT_RANGE / FLOAT_RANGE /
%%              STRING_RANGE / DATE_RANGE / BINARY_RANGE
range_list(T1, Type) ->
	range_list(T1, Type, []).


range_list(T1, Type, Acc1) ->
	T2 = strip(T1),
	{put, T3, F} = range_item(T2, Type),
	Acc2 = [F|Acc1],
	T4 = strip(T3),
	case T4 of
		";" ++ T5 ->
			Elem = #range{
				range_list=Acc2
			},
			{put, T5, Elem};
		"," ++ T5 ->
			range_list(T5, Type, Acc2)
	end.
	
	

%% DATE_RANGE   = (1*DIGIT / DATE_V) *1( ".." *(DIGIT / DATE_V) )
%% DATE_RANGE = ( DATE_DEF ".." ) / ( ".." DATE_DEF ) /
%%                  ( DATE_DEF ".." DATE_DEF )

range_item(T1, float) ->
	float_range(T1);

range_item(T1, int) ->
	int_range(T1);

range_item(T1, X) when X =:= binary; X =:= string; X =:= uint ->
	uint_range(T1).

% TODO: add date_range
%range_item(T1, date) ->
%	date_range(T1);



%% Unsigned integers is similar, but can not have its range open to
%% the left.
%% UINT_RANGE  = UINT_DEF *1( ".." UINT_DEF )
%% UINT_RANGE  = 1*DIGIT *1( ".." *DIGIT )
%% BINARY_RANGE = UINT_RANGE
%% STRING_RANGE = UINT_RANGE
uint_range(T1) ->
	{Num, T2} = read_uinteger_value(T1),
	case T2 of
		".." ++ T3 ->
			try
				{Num2, T4} = read_uinteger_value(T3),
				F = fun(X) -> X >= Num andalso X =< Num2 end,
				{put, T4, F}
			catch error:_ ->
				F1 = fun(X) -> X >= Num end,
				{put, T3, F1}
			end;

		_ ->
			F = fun(X) -> X =:= Num end,
			{put, T2, F}
	end.


%% INT_RANGE   = INT_V / ( INT_V ".." ) / ( ".." INT_V ) /
%%                ( INT_V ".." INT_V )
int_range(".." ++ T1) ->
	% .. INT
	{Num, T2} = read_integer_value(T1),
	F = fun(X) -> X >= Num end,
	{put, T2, F};

int_range(T1) ->
	{Num, T2} = read_integer_value(T1),
	case T2 of
	".." ++ T3 ->
		try
			{Num2, T4} = read_integer_value(T3),
			F = fun(X) -> X >= Num andalso X =< Num2 end,
			{put, T4, F}

		catch error:_ ->
			F1 = fun(X) -> X >= Num end,
			{put, T3, F1}
		end;

	_ ->
		F = fun(X) -> X =:= Num end,
		{put, T2, F}
	end.


%% FLOAT_RANGE = ( ("<" / "<=" / ">" / ">=") FLOAT_DEF ) /
%%                 ( FLOAT_DEF "<"/"<=" ".." "<"/"<=" FLOAT_DEF )
float_range(T1) ->
	case T1 of
		"<=" ++ T2 ->
			{Num, T3} = read_float_value(T2),
			F = fun(X) -> X =< Num end,
			{put, T3, F};

		"<"  ++ T2 ->
			{Num, T3} = read_float_value(T2),
			F = fun(X) -> X < Num end,
			{put, T3, F};

		">=" ++ T2 ->
			{Num, T3} = read_float_value(T2),
			F = fun(X) -> X >= Num end,
			{put, T3, F};

		">"  ++ T2 ->
			{Num, T3} = read_float_value(T2),
			F = fun(X) -> X > Num end,
			{put, T3, F};

		_ ->
			{Num, T2} = read_float_value(T1),
			true = is_number(Num),

			try
				% ( FLOAT_DEF "<"/"<=" ".." "<"/"<=" FLOAT_DEF )
				case T2 of
				"<=" ++ T3 -> Left = inclusive;
				"<"  ++ T3 -> Left = exclusive
				end,

				case T3 of
				"..<=" ++ T4 -> Right = inclusive;
				"..<"  ++ T4 -> Right = exclusive
				end,
			

				{Num2, T5} = read_value(T4),
				true = is_number(Num2),

				F = case {Left, Right} of
						{inclusive, inclusive} ->
							fun(X) -> X >= Num andalso X =< Num2 end;

						{exclusive, inclusive} ->
							fun(X) -> X > Num andalso X =< Num2 end;

						{inclusive, exclusive} ->
							fun(X) -> X >= Num andalso X < Num2 end;

						{exclusive, exclusive} ->
							fun(X) -> X > Num andalso X < Num2 end
					end,

				{put, T5, F}

		catch error:_ ->
			% FLOATs are exactly equals
			F1 = fun(X) -> X =:= Num end,
			{F1, T2}
		end
	end.

		
	
%% SIZE       = "size" S ":" S SIZE_LIST S ";"
%% SIZE_LIST  = UINT_RANGE / ( UINT_RANGE S "," S SIZE_LIST )
size_list(T1) ->
	{put, T2, Elem1} = range_list(T1, uint),
	Elem2 = #size{
		size_list=Elem1#range.range_list
	},
	{put, T2, Elem2}.





read_lcomment(T) ->
	read_lcomment(T, "").

read_bcomment(T) ->
	read_bcomment(T, "").

read_name(T) ->
	read_name(T, "").

read_type(T) ->
	read_name(T).

read_name([H|T], Acc)
	when H >= $a, H =< $z;
	     H >= $A, H =< $Z;
	     H >= $0, H =< $9, Acc =/= "" -> 
	read_name(T, [H|Acc]);

read_name(T, [_|_]=Acc) ->
	{list_to_atom(lists:reverse(Acc)), T}.
	
	

%% DEFS     = ( INT_DEF / UINT_DEF / FLOAT_DEF / STRING_DEF /
%%                 DATE_DEF / BINARY_DEF / NAME )

% Cannot parse DATE_DEF, because it has the same format with INT_DEF.
read_value([H|_]=X)
	when H =:= $x; H =:= $" -> 
	read_string_value(X);	

% read a number
read_value([H|_]=X)
	when H =:= $-; 
		 H >= 0; H =< 9 -> 
	try
		read_float_value(X)
	catch
		error:{badmatch,{error,{fread,float}}} ->
		try
			read_integer_value(X)
		catch error:_ ->
			erlang:throw({bad_value, X})
		end
	end;

read_value(X) ->
	read_name(X).

read_id(T1) -> 
	{ok, [Dec], T2} = io_lib:fread("~16u", T1),
	{Dec, T2}.


read_value(T1, Type) ->
	try
		case Type of
		float ->
			read_float_value(T1);
		date ->
			read_date_value(T1);
		uint ->
			read_uinteger_value(T1);
		int ->
			read_integer_value(T1);
		binary ->
			read_string_value(T1);
		string ->
			read_string_value(T1)
		end
	catch error:_ -> 
		read_name(T1)
	end.

	


%%  ExampleFloat := c2 float [ def:6.022E23 ]
read_float_value(T1) -> 
	{ok, [Float], T2} = io_lib:fread("~f", T1),
	{Float, T2}.

%% ExampleInt := c0 int [ def:-114; ]
read_integer_value(T1) -> 
	{ok, [Dec], T2} = io_lib:fread("~d", T1),
	{Dec, T2}.

%% ExampleUInt := c1 uint [ def:0; ]
read_uinteger_value(T1) -> 
	{ok, [Dec], T2} = io_lib:fread("~u", T1),
	{Dec, T2}.


%% Transforms date from "yyyymmddThh:mm:ss.f" to DATE.
%%
%% DATE = 8BYTE
%%
%%   Signed, 64-bit (8 byte) integer describing the distance in
%%   nanoseconds to the beginning of the millennium (2001-01-01 00:00:00
%%   UTC).
%%
%% ExampleDate := c3 date [ def:20011224T15:00:03.21; ]
read_date_value(T1) ->
	% YYYY MM DD ($T Hh Mm : Ss (. Ff))
	{ok, [YYYY, MM, DD], T2} = io_lib:fread("~4d~2d~2d", T1),
	{Hh, Mm, Ss, Ff, T3} = case T2 of
			[$T|T2a] -> read_time_value(T2a);
			_ -> {0, 0, 0, 0, T2}
		end,


	MilleniumSs = calendar:datetime_to_gregorian_seconds({{2001,1,1},{0,0,0}}),
	ValueTimeStamp = {{YYYY, MM, DD}, {Hh, Mm, Ss}},
	ValueSs = calendar:datetime_to_gregorian_seconds(ValueTimeStamp),

	DATE = (ValueSs - MilleniumSs) * 1000000000 + Ff,
	{DATE, T3}.



read_time_value(T1) ->
	{ok, [Hh, Mm, Ss], T2} = io_lib:fread("~2d:~2d:~2d", T1),

	case T2 of
	[$.|T3] ->
		{ok, [Ff], T4} = io_lib:fread("~f", "0." ++ T3),
		% Ff is in seconds. Convert to nanoseconds.
		NanoFf = erlang:trunc(Ff * 10.0e+9),
		{Hh, Mm, Ss, NanoFf, T4};

	_ ->
		{Hh, Mm, Ss, 0, T2}
	end.
		
		
-spec read_string_value(string()) -> {binary(), string()}.

read_string_value([$x|T1]) ->
	read_hex_string_value(T1);

read_string_value([$"|T1]) ->
	read_plain_string_value(T1, "").

%% ExampleBinary := c5 binary [ def:0x4944337632; ]
read_hex_string_value(T1) ->
	{ok, [Int], T2} = io_lib:fread("~16u", T1),
	{binary:encode_unsigned(Int), T2}.
	
	
%% ExampleString := c6 string [ def:"Sweden"; ]
read_plain_string_value([$"|T], Acc) ->
	{list_to_binary(lists:reverse(Acc)), T};

read_plain_string_value([H|T], Acc) 
	when H >= 16#20, H =< 16#7e ->
	read_plain_string_value(T, [H|Acc]).




read_lcomment("\n" ++ T, Acc) ->
	{lists:reverse(Acc), T};

read_lcomment([H|T], Acc) ->
	read_lcomment(T, [H|Acc]);

read_lcomment([], Acc) ->
	{lists:reverse(Acc), []}.


read_bcomment("*/" ++ T, Acc) ->
	{lists:reverse(Acc), T};

read_bcomment([H|T], Acc) ->
	read_bcomment(T, [H|Acc]).



load_matroska_dtd() ->
	Dir = code:priv_dir('ebml'),
	{ok, Bin} = file:read_file(Dir ++ "/matroska.edtd"),
	binary_to_list(Bin).

dbg() ->
	dbg:tracer(),
	dbg:p(self(), [c]),
	dbg:tpl({?MODULE, '_', '_'}, x),
	parse(load_matroska_dtd()).
	
	
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(M, ?MODULE).

read_date_value_test_() ->
	F = fun read_date_value/1,
	[?_assertEqual(F("20010101"), {0, ""})
	].

read_string_value_test_() ->
	F = fun read_string_value/1,
	[?_assertEqual(F("x4944337632"), {<<"ID3v2">>, ""})
	,?_assertEqual(F("\"x4944337632\" "), {<<"x4944337632">>, " "})
	,?_assertEqual(F("\"Sweden\""), {<<"Sweden">>, ""})
	].

parse_test() ->
	Header = "declare header {
    	 DocType := \"matroska\";
	     EBMLVersion := 1;
	   }",

	Types = "define types {
	     bool := uint [ range:0..1; ]
	     ascii := string [ range:32..126; ]
	   }",

	parse(Header),
	parse(Types),
	io:write(user, parse(Header ++ " " ++ Types)).

load_test() ->
	List = load_matroska_dtd(),
	parse(List).

-endif.
