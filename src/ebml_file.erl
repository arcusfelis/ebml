-module(ebml_file).
-compile([export_all]).

% ebml_integer:decode(binary:encode_unsigned(16#1f43b675))
% Cluster 1f43b675
-define('CLUSTER_ID', 256095861).

% SeekHead 114d9b74
-define('SEEK_HEAD_ID', 21863284).

% Cues 1c53bb6b
-define('CUES_ID', 206814059).

% Segment
-define('SEGMENT_ID', 139690087).

% Tracks
-define('TRACKS_ID', 106212971).

% TrackEntry
-define('TRACK_ENTRY_ID', 46).


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
    value
}).

open(FileName) ->
	Modes = [read, binary],
	file:open(FileName, Modes).

id(Id) ->
    fun(#header{id=X}) ->
        Id =:= X
    end.

find_subtitles(Fd) ->
    [_EBML, Matroska] = read_root_headers(Fd),
    % Matroska is a Segment.
    [Tracks]  = read_children_headers(Fd, Matroska, id(?TRACKS_ID)),
    Headers   = read_children_headers(Fd, Tracks, id(?TRACK_ENTRY_ID)),
    [ header_to_element(Fd, X) || X <- Headers ].
    

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

read_children_headers(Fd, #header{size=S, offset=O, position=P}, V) ->
    read_headers(V, Fd, P+O, P+O+S, []).


header_to_element(Fd, Head=#header{id=Id}) ->
    Type = ebml_dtd:id_to_type(Id),

    Value = case Type of
            container ->
                read_children(Fd, Head);
            binary ->
                binary;
            string ->
                read_binary(Fd, Head);
            date ->
                decode_int(read_binary(Fd, Head));
            int ->
                decode_int(read_binary(Fd, Head));
            uint ->
                decode_uint(read_binary(Fd, Head));
            float ->
                decode_float(read_binary(Fd, Head));
            unknown ->
                unknown
        end,

    #element{
        header = Head,
        name = ebml_dtd:id_to_name(Id),
        type = Type,
        value = Value
    }.
        

decode_float(Bin) ->
    Len = bit_size(Bin),
    <<X:Len/float-big>> = Bin,
    X.
        

decode_uint(Bin) ->
    Len = bit_size(Bin),
    <<X:Len/unsigned-integer-big>> = Bin,
    X.
        

decode_int(Bin) ->
    Len = bit_size(Bin),
    <<X:Len/signed-integer-big>> = Bin,
    X.

%% A set of track types coded on 8 bits 
%% (1: video, 2: audio, 3: complex, 
%%  0x10: logo, 0x11: subtitle, 0x12: buttons, 0x20: control).
track_type(1) ->
    video;
track_type(2) ->
    audio;
track_type(3) ->
    complex;
track_type(16#10) ->
    logo;
track_type(16#11) ->
    subtitle;
track_type(16#12) ->
    buttons;
track_type(16#20) ->
    control.


read_binary(_Fd, #header{size=S}) when S>120 ->
    too_long;

read_binary(Fd, Head=#header{size=S, offset=O, position=P}) ->
    {ok, Last} = file:position(Fd, {bof, P+O}),
	{ok, Bin} = file:read(Fd, S),
    Bin.


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
	%read_all(Fd).
    find_subtitles(Fd).
