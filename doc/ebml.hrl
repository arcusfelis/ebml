#dtd{
    hblock = false,tblock = false,
    eblock = 
        #eblock{
            elements = 
                [#delement{
                     name = 'EBML',id = 172351395,type = container,
                     properties = [#card{pattern = '+'}],
                     elements = 
                         [#delement{
                              name = 'EBMLVersion',id = 646,type = uint,
                              properties = [#def{value = 1}],
                              elements = []},
                          #delement{
                              name = 'EBMLReadVersion',id = 759,type = uint,
                              properties = [#def{value = 1}],
                              elements = []},
                          #delement{
                              name = 'EBMLMaxIDLength',id = 754,type = uint,
                              properties = [#def{value = 4}],
                              elements = []},
                          #delement{
                              name = 'EBMLMaxSizeLength',id = 755,type = uint,
                              properties = [#def{value = 8}],
                              elements = []},
                          #delement{
                              name = 'DocType',id = 642,type = string,
                              properties = [#range{range_list = [{'[',32,126,']'}]}],
                              elements = []},
                          #delement{
                              name = 'DocTypeVersion',id = 647,type = uint,
                              properties = [#def{value = 1}],
                              elements = []},
                          #delement{
                              name = 'DocTypeReadVersion',id = 645,type = uint,
                              properties = [#def{value = 1}],
                              elements = []}]},
                 #delement{
                     name = 'CRC32',id = 67,type = container,
                     properties = [#level{from = 1,to = 1},#card{pattern = '*'}],
                     elements = 
                         [children,
                          #delement{
                              name = 'CRC32Value',id = 766,type = binary,
                              properties = [#size{size_list = [{'[',4,4,']'}]}],
                              elements = []}]},
                 #delement{
                     name = 'Void',id = 108,type = binary,
                     properties = [#level{from = 1,to = 1},#card{pattern = '*'}],
                     elements = []}]}}
