-module(ebml_file).
-compile([export_all]).

open(FileName) ->
	Modes = [read, binary],
	file:open(FileName, Modes).

read_element_head(Fd) ->
	{ok, Bin} = file:read(Fd, 32),
	decode_element_head(Bin).

read_all(Fd) ->
	read_all(Fd, 0, []).

read_all(Fd, Pos, Acc1) ->
	% Move on Pos
	{ok, Pos} = file:position(Fd, {bof, Pos}),
	Head = read_element_head(Fd),
	{ok, DOffset, Id, Size} = Head, 
	Acc2 = [Id|Acc1],
	NextPartPos = Pos + DOffset + Size,
	io:write(user, {NextPartPos, Head}),
	read_all(Fd, NextPartPos, Acc2).


%position()

%%   EBML       = *ELEMENT
%%   ELEMENT    = ELEMENT_ID SIZE DATA
%%   DATA       = VALUE / *ELEMENT
%%   ELEMENT_ID = VINT
%%   SIZE       = VINT

decode_element_head(Bin) ->
	io:write(user, Bin),
	{Offset1, Id,   Bin2}  = ebml_integer:decode(Bin),
	{Offset2, Size, _Bin3} = ebml_integer:decode(Bin2),
	DataOffset = Offset1 + Offset2,
	{ok, DataOffset, Id, Size}.

test() ->
	FileName = "/media/poltarashka/Anime/Koty.aristokraty.1970.HDTVRip(1080p).mkv",
	{ok, Fd} = open(FileName),
	read_all(Fd).
