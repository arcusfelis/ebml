#dtd{
 hblock = 
  #hblock{
   headers = 
    [#statement{name = 'DocType',value = <<"matroska">>},
     #statement{name = 'EBMLVersion',value = 1}]},
 tblock = 
  #tblock{
   types = 
    [#dtype{
      name = bool,type = uint,
      properties = [#range{range_list = [{'[',0,1,']'}]}]},
     #dtype{
      name = ascii,type = string,
      properties = [#range{range_list = [{'[',32,126,']'}]}]}]},
 eblock = 
  #eblock{
   elements = 
    [#delement{
      name = 'Segment',id = 139690087,type = container,
      properties = [#card{pattern = '*'}],
      elements = 
       [#delement{
         name = 'SeekHead',id = 21863284,type = container,
         properties = [#card{pattern = '*'}],
         elements = 
          [#delement{
            name = 'Seek',id = 3515,type = container,
            properties = [#card{pattern = '*'}],
            elements = 
             [#delement{
               name = 'SeekID',id = 5035,type = binary,properties = [],
               elements = []},
              #delement{
               name = 'SeekPosition',id = 5036,type = uint,properties = [],
               elements = []}]}]},
        #delement{
         name = 'Info',id = 88713574,type = container,
         properties = [#card{pattern = '*'}],
         elements = 
          [#delement{
            name = 'SegmentUID',id = 13220,type = binary,
            properties = [],elements = []},
           #delement{
            name = 'SegmentFilename',id = 13188,type = string,
            properties = [],elements = []},
           #delement{
            name = 'PrevUID',id = 1882403,type = binary,properties = [],
            elements = []},
           #delement{
            name = 'PrevFilename',id = 1868715,type = string,
            properties = [],elements = []},
           #delement{
            name = 'NextUID',id = 2013475,type = binary,properties = [],
            elements = []},
           #delement{
            name = 'NextFilename',id = 1999803,type = string,
            properties = [],elements = []},
           #delement{
            name = 'TimecodeScale',id = 710577,type = uint,
            properties = [#def{value = 1000000}],
            elements = []},
           #delement{
            name = 'Duration',id = 1161,type = float,
            properties = [#range{range_list = [{'(',0.0,inf,')'}]}],
            elements = []},
           #delement{
            name = 'DateUTC',id = 1121,type = date,properties = [],
            elements = []},
           #delement{
            name = 'Title',id = 15273,type = string,properties = [],
            elements = []},
           #delement{
            name = 'MuxingApp',id = 3456,type = string,properties = [],
            elements = []},
           #delement{
            name = 'WritingApp',id = 5953,type = string,properties = [],
            elements = []}]},
        #delement{
         name = 'Cluster',id = 256095861,type = container,
         properties = [#card{pattern = '*'}],
         elements = 
          [#delement{
            name = 'Timecode',id = 103,type = uint,properties = [],
            elements = []},
           #delement{
            name = 'Position',id = 39,type = uint,properties = [],
            elements = []},
           #delement{
            name = 'PrevSize',id = 43,type = uint,properties = [],
            elements = []},
           #delement{
            name = 'BlockGroup',id = 32,type = container,
            properties = [#card{pattern = '*'}],
            elements = 
             [#delement{
               name = 'Block',id = 33,type = binary,properties = [],
               elements = []},
              #delement{
               name = 'BlockVirtual',id = 34,type = binary,properties = [],
               elements = []},
              #delement{
               name = 'BlockAdditions',id = 13729,type = container,
               properties = [],
               elements = 
                [#delement{
                  name = 'BlockMore',id = 38,type = container,
                  properties = [#card{pattern = '*'}],
                  elements = 
                   [#delement{
                     name = 'BlockAddID',id = 110,type = uint,
                     properties = [#range{range_list = [{'[',1,inf,')'}]}],
                     elements = []},
                    #delement{
                     name = 'BlockAdditional',id = 37,type = binary,
                     properties = [],elements = []}]}]},
              #delement{
               name = 'BlockDuration',id = 27,type = uint,
               properties = [#def{value = 'TrackDuration'}],
               elements = []},
              #delement{
               name = 'ReferencePriority',id = 122,type = uint,
               properties = [],elements = []},
              #delement{
               name = 'ReferenceBlock',id = 123,type = int,
               properties = [#card{pattern = '*'}],
               elements = []},
              #delement{
               name = 'ReferenceVirtual',id = 125,type = int,
               properties = [],elements = []},
              #delement{
               name = 'CodecState',id = 36,type = binary,properties = [],
               elements = []},
              #delement{
               name = 'Slices',id = 14,type = container,
               properties = [#card{pattern = '*'}],
               elements = 
                [#delement{
                  name = 'TimeSlice',id = 104,type = container,
                  properties = [#card{pattern = '*'}],
                  elements = 
                   [#delement{
                     name = 'LaceNumber',id = 76,type = uint,
                     properties = [#def{value = 0}],
                     elements = []},
                    #delement{
                     name = 'FrameNumber',id = 77,type = uint,
                     properties = [#def{value = 0}],
                     elements = []},
                    #delement{
                     name = 'BlockAdditionID',id = 75,type = uint,
                     properties = [#def{value = 0}],
                     elements = []},
                    #delement{
                     name = 'Delay',id = 78,type = uint,
                     properties = [#def{value = 0}],
                     elements = []},
                    #delement{
                     name = 'Duration',id = 79,type = uint,
                     properties = [#def{value = 'TrackDuration'}],
                     elements = []}]}]}]}]},
        #delement{
         name = 'Tracks',id = 106212971,type = container,
         properties = [#card{pattern = '*'}],
         elements = 
          [#delement{
            name = 'TrackEntry',id = 46,type = container,
            properties = [#card{pattern = '*'}],
            elements = 
             [#delement{
               name = 'TrackNumber',id = 87,type = uint,
               properties = [#range{range_list = [{'[',1,inf,')'}]}],
               elements = []},
              #delement{
               name = 'TrackUID',id = 13253,type = uint,
               properties = [#range{range_list = [{'[',1,inf,')'}]}],
               elements = []},
              #delement{
               name = 'TrackType',id = 3,type = uint,
               properties = [#range{range_list = [{'[',1,254,']'}]}],
               elements = []},
              #delement{
               name = 'FlagEnabled',id = 57,type = uint,
               properties = 
                [#range{range_list = [{'[',0,1,']'}]},#def{value = 1}],
               elements = []},
              #delement{
               name = 'FlagDefault',id = 8,type = uint,
               properties = 
                [#range{range_list = [{'[',0,1,']'}]},#def{value = 1}],
               elements = []},
              #delement{
               name = 'FlagLacing',id = 28,type = uint,
               properties = 
                [#range{range_list = [{'[',0,1,']'}]},#def{value = 1}],
               elements = []},
              #delement{
               name = 'MinCache',id = 11751,type = uint,
               properties = [#def{value = 0}],
               elements = []},
              #delement{
               name = 'MaxCache',id = 11768,type = uint,properties = [],
               elements = []},
              #delement{
               name = 'DefaultDuration',id = 254851,type = uint,
               properties = [#range{range_list = [{'[',1,inf,')'}]}],
               elements = []},
              #delement{
               name = 'TrackTimecodeScale',id = 209231,type = float,
               properties = 
                [#range{range_list = [{'(',0.0,inf,')'}]},#def{value = 1.0}],
               elements = []},
              #delement{
               name = 'Name',id = 4974,type = string,properties = [],
               elements = []},
              #delement{
               name = 'Language',id = 177564,type = string,
               properties = 
                [#def{value = <<"eng">>},
                 #range{range_list = [{'[',32,126,']'}]}],
               elements = []},
              #delement{
               name = 'CodecID',id = 6,type = string,
               properties = [#range{range_list = [{'[',32,126,']'}]}],
               elements = []},
              #delement{
               name = 'CodecPrivate',id = 9122,type = binary,
               properties = [],elements = []},
              #delement{
               name = 'CodecName',id = 362120,type = string,
               properties = [],elements = []},
              #delement{
               name = 'CodecSettings',id = 1742487,type = string,
               properties = [],elements = []},
              #delement{
               name = 'CodecInfoURL',id = 1785920,type = string,
               properties = 
                [#card{pattern = '*'},
                 #range{range_list = [{'[',32,126,']'}]}],
               elements = []},
              #delement{
               name = 'CodecDownloadURL',id = 438848,type = string,
               properties = 
                [#card{pattern = '*'},
                 #range{range_list = [{'[',32,126,']'}]}],
               elements = []},
              #delement{
               name = 'CodecDecodeAll',id = 42,type = uint,
               properties = 
                [#range{range_list = [{'[',0,1,']'}]},#def{value = 1}],
               elements = []},
              #delement{
               name = 'TrackOverlay',id = 12203,type = uint,
               properties = [],elements = []},
              #delement{
               name = 'Video',id = 96,type = container,properties = [],
               elements = 
                [#delement{
                  name = 'FlagInterlaced',id = 26,type = uint,
                  properties = 
                   [#range{range_list = [{'[',0,1,']'}]},#def{value = 0}],
                  elements = []},
                 #delement{
                  name = 'StereoMode',id = 5048,type = uint,
                  properties = 
                   [#range{range_list = [{'[',0,3,']'}]},#def{value = 0}],
                  elements = []},
                 #delement{
                  name = 'PixelWidth',id = 48,type = uint,
                  properties = [#range{range_list = [{'[',1,inf,')'}]}],
                  elements = []},
                 #delement{
                  name = 'PixelHeight',id = 58,type = uint,
                  properties = [#range{range_list = [{'[',1,inf,')'}]}],
                  elements = []},
                 #delement{
                  name = 'DisplayWidth',id = 5296,type = uint,
                  properties = [#def{value = 'PixelWidth'}],
                  elements = []},
                 #delement{
                  name = 'DisplayHeight',id = 5306,type = uint,
                  properties = [#def{value = 'PixelHeight'}],
                  elements = []},
                 #delement{
                  name = 'DisplayUnit',id = 5298,type = uint,
                  properties = [#def{value = 0}],
                  elements = []},
                 #delement{
                  name = 'AspectRatioType',id = 5299,type = uint,
                  properties = [#def{value = 0}],
                  elements = []},
                 #delement{
                  name = 'ColourSpace',id = 963876,type = binary,
                  properties = [],elements = []},
                 #delement{
                  name = 'GammaValue',id = 1029411,type = float,
                  properties = [#range{range_list = [{'(',0.0,inf,')'}]}],
                  elements = []}]},
              #delement{
               name = 'Audio',id = 97,type = container,properties = [],
               elements = 
                [#delement{
                  name = 'SamplingFrequency',id = 53,type = float,
                  properties = 
                   [#range{range_list = [{'(',0.0,inf,')'}]},
                    #def{value = 8.0e3}],
                  elements = []},
                 #delement{
                  name = 'OutputSamplingFrequency',id = 14517,type = float,
                  properties = 
                   [#range{range_list = [{'(',0.0,inf,')'}]},
                    #def{value = 8.0e3}],
                  elements = []},
                 #delement{
                  name = 'Channels',id = 20,type = uint,
                  properties = 
                   [#range{range_list = [{'[',1,inf,')'}]},#def{value = 1}],
                  elements = []},
                 #delement{
                  name = 'ChannelPositions',id = 15739,type = binary,
                  properties = [],elements = []},
                 #delement{
                  name = 'BitDepth',id = 8804,type = uint,
                  properties = [#range{range_list = [{'[',1,inf,')'}]}],
                  elements = []}]},
              #delement{
               name = 'ContentEncodings',id = 11648,type = container,
               properties = [],
               elements = 
                [#delement{
                  name = 'ContentEncoding',id = 8768,type = container,
                  properties = [#card{pattern = '*'}],
                  elements = 
                   [#delement{
                     name = 'ContentEncodingOrder',id = 4145,type = uint,
                     properties = [#def{value = 0}],
                     elements = []},
                    #delement{
                     name = 'ContentEncodingScope',id = 4146,type = uint,
                     properties = 
                      [#range{range_list = [{'[',1,inf,')'}]},#def{value = 1}],
                     elements = []},
                    #delement{
                     name = 'ContentEncodingType',id = 4147,type = uint,
                     properties = [],elements = []},
                    #delement{
                     name = 'ContentCompression',id = 4148,type = container,
                     properties = [],
                     elements = 
                      [#delement{
                        name = 'ContentCompAlgo',id = 596,type = uint,
                        properties = [#def{value = 0}],
                        elements = []},
                       #delement{
                        name = 'ContentCompSettings',id = 597,type = binary,
                        properties = [],elements = []}]},
                    #delement{
                     name = 'ContentEncryption',id = 4149,type = container,
                     properties = [],
                     elements = 
                      [#delement{
                        name = 'ContentEncAlgo',id = 2017,type = uint,
                        properties = [#def{value = 0}],
                        elements = []},
                       #delement{
                        name = 'ContentEncKeyID',id = 2018,type = binary,
                        properties = [],elements = []},
                       #delement{
                        name = 'ContentSignature',id = 2019,type = binary,
                        properties = [],elements = []},
                       #delement{
                        name = 'ContentSigKeyID',id = 2020,type = binary,
                        properties = [],elements = []},
                       #delement{
                        name = 'ContentSigAlgo',id = 2021,type = uint,
                        properties = [],elements = []},
                       #delement{
                        name = 'ContentSigHashAlgo',id = 2022,type = uint,
                        properties = [],elements = []}]}]}]}]}]},
        #delement{
         name = 'Cues',id = 206814059,type = container,
         properties = [],
         elements = 
          [#delement{
            name = 'CuePoint',id = 59,type = container,
            properties = [#card{pattern = '*'}],
            elements = 
             [#delement{
               name = 'CueTime',id = 51,type = uint,properties = [],
               elements = []},
              #delement{
               name = 'CueTrackPositions',id = 55,type = container,
               properties = [#card{pattern = '*'}],
               elements = 
                [#delement{
                  name = 'CueTrack',id = 119,type = uint,
                  properties = [#range{range_list = [{'[',1,inf,')'}]}],
                  elements = []},
                 #delement{
                  name = 'CueClusterPosition',id = 113,type = uint,
                  properties = [],elements = []},
                 #delement{
                  name = 'CueBlockNumber',id = 4984,type = uint,
                  properties = 
                   [#range{range_list = [{'[',1,inf,')'}]},#def{value = 1}],
                  elements = []},
                 #delement{
                  name = 'CueCodecState',id = 106,type = uint,
                  properties = [#def{value = 0}],
                  elements = []},
                 #delement{
                  name = 'CueReference',id = 91,type = container,
                  properties = [#card{pattern = '*'}],
                  elements = 
                   [#delement{
                     name = 'CueRefTime',id = 22,type = uint,properties = [],
                     elements = []},
                    #delement{
                     name = 'CueRefCluster',id = 23,type = uint,properties = [],
                     elements = []},
                    #delement{
                     name = 'CueRefNumber',id = 4959,type = uint,
                     properties = 
                      [#range{range_list = [{'[',1,inf,')'}]},#def{value = 1}],
                     elements = []},
                    #delement{
                     name = 'CueRefCodecState',id = 107,type = uint,
                     properties = [#def{value = 0}],
                     elements = []}]}]}]}]},
        #delement{
         name = 'Attachments',id = 155296873,type = container,
         properties = [],
         elements = 
          [#delement{
            name = 'AttachedFile',id = 8615,type = container,
            properties = [#card{pattern = '*'}],
            elements = 
             [#delement{
               name = 'FileDescription',id = 1662,type = string,
               properties = [],elements = []},
              #delement{
               name = 'FileName',id = 1646,type = string,properties = [],
               elements = []},
              #delement{
               name = 'FileMimeType',id = 1632,type = string,
               properties = [#range{range_list = [{'[',32,126,']'}]}],
               elements = []},
              #delement{
               name = 'FileData',id = 1628,type = binary,properties = [],
               elements = []},
              #delement{
               name = 'FileUID',id = 1710,type = uint,properties = [],
               elements = []}]}]},
        #delement{
         name = 'Chapters',id = 4433776,type = container,
         properties = [],
         elements = 
          [#delement{
            name = 'EditionEntry',id = 1465,type = container,
            properties = [#card{pattern = '*'}],
            elements = 
             [#delement{
               name = 'ChapterAtom',id = 54,type = container,
               properties = [#card{pattern = '*'}],
               elements = 
                [#delement{
                  name = 'ChapterUID',id = 13252,type = uint,
                  properties = [#range{range_list = [{'[',1,inf,')'}]}],
                  elements = []},
                 #delement{
                  name = 'ChapterTimeStart',id = 17,type = uint,
                  properties = [],elements = []},
                 #delement{
                  name = 'ChapterTimeEnd',id = 18,type = uint,properties = [],
                  elements = []},
                 #delement{
                  name = 'ChapterFlagHidden',id = 24,type = uint,
                  properties = 
                   [#range{range_list = [{'[',0,1,']'}]},#def{value = 0}],
                  elements = []},
                 #delement{
                  name = 'ChapterFlagEnabled',id = 1432,type = uint,
                  properties = 
                   [#range{range_list = [{'[',0,1,']'}]},#def{value = 0}],
                  elements = []},
                 #delement{
                  name = 'ChapterTrack',id = 15,type = container,
                  properties = [],
                  elements = 
                   [#delement{
                     name = 'ChapterTrackNumber',id = 9,type = uint,
                     properties = 
                      [#card{pattern = '*'},#range{range_list = [{'[',0,1,']'}]}],
                     elements = []},
                    #delement{
                     name = 'ChapterDisplay',id = 0,type = container,
                     properties = [#card{pattern = '*'}],
                     elements = 
                      [#delement{
                        name = 'ChapString',id = 5,type = string,properties = [],
                        elements = []},
                       #delement{
                        name = 'ChapLanguage',id = 892,type = string,
                        properties = 
                         [#card{pattern = '*'},
                          #def{value = <<"eng">>},
                          #range{range_list = [{'[',32,126,']'}]}],
                        elements = []},
                       #delement{
                        name = 'ChapCountry',id = 894,type = string,
                        properties = 
                         [#card{pattern = '*'},
                          #range{range_list = [{'[',32,126,']'}]}],
                        elements = []}]}]}]}]}]},
        #delement{
         name = 'Tags',id = 39109479,type = container,
         properties = [#card{pattern = '*'}],
         elements = 
          [#delement{
            name = 'Tag',id = 13171,type = container,
            properties = [#card{pattern = '*'}],
            elements = 
             [#delement{
               name = 'Targets',id = 9152,type = container,properties = [],
               elements = 
                [#delement{
                  name = 'TrackUID',id = 9157,type = uint,
                  properties = [#card{pattern = '*'},#def{value = 0}],
                  elements = []},
                 #delement{
                  name = 'ChapterUID',id = 9156,type = uint,
                  properties = [#card{pattern = '*'},#def{value = 0}],
                  elements = []},
                 #delement{
                  name = 'AttachmentUID',id = 9158,type = uint,
                  properties = [#card{pattern = '*'},#def{value = 0}],
                  elements = []}]},
              #delement{
               name = 'SimpleTag',id = 10184,type = container,
               properties = [#card{pattern = '*'}],
               elements = 
                [#delement{
                  name = 'TagName',id = 1443,type = string,properties = [],
                  elements = []},
                 #delement{
                  name = 'TagString',id = 1159,type = string,properties = [],
                  elements = []},
                 #delement{
                  name = 'TagBinary',id = 1157,type = binary,properties = [],
                  elements = []}]}]}]}]}]}}

