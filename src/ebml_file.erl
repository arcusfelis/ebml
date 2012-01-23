-module(ebml_file).
-compile([export_all]).

% ebml_integer:decode(binary:encode_unsigned(16#1f43b675))
% Cluster 1f43b675
-define('CLUSTER_ID', 256095861).

% SeekHead 114d9b74
-define('SEEK_HEAD_ID', 21863284).

% Cues 1c53bb6b
-define('CUES_ID', 206814059).

-record(header, {
    id,
    size,
    offset,
    position
}).

-record(element, {
    header,
    name,
    type,
    children
}).

open(FileName) ->
	Modes = [read, binary],
	file:open(FileName, Modes).


read_all(Fd) ->
    List = read_root_headers(Fd),
    [ header_to_element(Fd, X) || X <- List ].


%% @doc Read elements from the container with passed #header{}.
read_children(Fd, Parent) ->
    List = read_children_headers(Fd, Parent),
    [ header_to_element(Fd, X) || X <- List ].


read_children_headers(Fd, #header{size=S, offset=O, position=P}) ->
    V = fun(#header{id=Id}) -> 
            Id =/= ?CLUSTER_ID andalso 
            Id =/= ?SEEK_HEAD_ID andalso
            Id =/= ?CUES_ID 
        end,
    read_headers(V, Fd, P+O, P+O+S, []).


header_to_element(Fd, Head=#header{id=Id}) ->
    Type = ebml_dtd:id_to_type(Id),

    Childrens = case Type of
            container ->
                read_children(Fd, Head);
            _ -> []
        end,

    #element{
        header = Head,
        name = ebml_dtd:id_to_name(Id),
        type = Type,
        children = Childrens
    }.
        


read_root_headers(Fd) ->
    {ok, Last} = file:position(Fd, {eof, 0}),
    V = fun(_) -> true end,
	read_headers(V, Fd, 0, Last, []).


read_headers(_V, _Fd, Last, Last, Acc1) ->
    lists:reverse(Acc1);

read_headers(V, Fd, Pos, Last, Acc1) ->
	% Move on Pos
	{ok, Pos} = file:position(Fd, {bof, Pos}),
	Head = read_element_header(Fd),
	{ok, DOffset, Id, Size} = Head, 
    
    Elem = #header {
        id = Id,
        size = Size,
        offset = DOffset,
        position = Pos
    },

    % Validate element.
	Acc2 = case V(Elem) of 
            true -> [Elem|Acc1];
            false -> Acc1
        end,
	NextPartPos = Pos + DOffset + Size,
	read_headers(V, Fd, NextPartPos, Last, Acc2).


read_element_header(Fd) ->
	{ok, Bin} = file:read(Fd, 32),
	decode_element_header(Bin).


%position()

%%   EBML       = *ELEMENT
%%   ELEMENT    = ELEMENT_ID SIZE DATA
%%   DATA       = VALUE / *ELEMENT
%%   ELEMENT_ID = VINT
%%   SIZE       = VINT

decode_element_header(Bin) ->
	{Offset1, Id,    Bin2} = ebml_integer:decode(Bin),
	{Offset2, Size, _Bin3} = ebml_integer:decode(Bin2),
	DataOffset = Offset1 + Offset2,
	{ok, DataOffset, Id, Size}.


test() ->
	FileName = "/home/user/Videos/Higashi no Eden/"
        "[Frostii]_Eden_of_the_East_-_01_[720p][03B976E4].mkv",
	{ok, Fd} = open(FileName),
	read_all(Fd).
