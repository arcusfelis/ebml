-module(ebml_file).
-compile([export_all]).

-record(elem, {
    id,
    size,
    offset,
    position
}).

open(FileName) ->
	Modes = [read, binary],
	file:open(FileName, Modes).

read_element_head(Fd) ->
	{ok, Bin} = file:read(Fd, 32),
	decode_element_head(Bin).

read_all(Fd) ->
    {ok, Last} = file:position(Fd, {eof, 0}),
	read_all(Fd, 0, Last, []).


read_all(Fd, Last, Last, Acc1) ->
    lists:reverse(Acc1);

read_all(Fd, Pos, Last, Acc1) ->
	% Move on Pos
	{ok, Pos} = file:position(Fd, {bof, Pos}),
	Head = read_element_head(Fd),
	{ok, DOffset, Id, Size} = Head, 
    Elem = #elem {
        id = Id,
        size = Size,
        offset = DOffset,
        position = Pos
    },
	Acc2 = [Elem|Acc1],
	NextPartPos = Pos + DOffset + Size,
	io:write(user, {NextPartPos, Head}),
	read_all(Fd, NextPartPos, Last, Acc2).


%position()

%%   EBML       = *ELEMENT
%%   ELEMENT    = ELEMENT_ID SIZE DATA
%%   DATA       = VALUE / *ELEMENT
%%   ELEMENT_ID = VINT
%%   SIZE       = VINT

decode_element_head(Bin) ->
	{Offset1, Id,   Bin2}  = ebml_integer:decode(Bin),
	{Offset2, Size, _Bin3} = ebml_integer:decode(Bin2),
	DataOffset = Offset1 + Offset2,
	{ok, DataOffset, Id, Size}.

test() ->
	FileName = "/home/user/Videos/Higashi no Eden/"
        "[Frostii]_Eden_of_the_East_-_01_[720p][03B976E4].mkv",
	{ok, Fd} = open(FileName),
	read_all(Fd).
