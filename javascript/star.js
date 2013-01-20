var Star = (function(PC, $) {
    "use strict";

    function safeMap(f, xs) {
        var i = 0,
            out = [];
        for(; i < xs.length; i++) {
            out.push(f(xs[i]));
        }
        return out;
    }
    
	//  Newline   :=  '\n'  |  '\r'  |  '\f'
	//  Blank     :=  ' '  |  '\t'
	//  Space     :=  Blank  |  Newline
	//  Special   :=  '"'  |  '#'  |  '_'  |  Blank  |  Newline
    
    var newline = PC.any(safeMap(PC.literal, '\n\r\f')),
        blank = PC.literal(' ').plus(PC.literal('\t')),
        space = blank.plus(newline),
        special = PC.any(safeMap(PC.literal, '"#_')).plus(space);
	
	//  Stop      :=  "stop_"
	//  SaveClose :=  "save_"
	//  Loop      :=  "loop_"
	//  Comment   :=  '#'  (not Newline)(*)
	//  DataOpen  :=  "data_"  (not Space)(+)
	//  SaveOpen  :=  "save_"  (not Space)(+)
    
    var stop = PC.string("stop_"),
        saveClose = PC.string("save_"),
        loop = PC.string("loop_"),
        comment = PC.literal('#').seq2R(newline.not1().many0()),
        dataOpen = PC.string("data_").seq2R(space.not1().many1()),
        saveOpen = saveClose.seq2R(space.not1().many1());
	
	//  Identifier :=  '_'  (not Space)(+)
    var identifier = PC.literal('_').seq2R(space.not1().many1());
	
	//  Unquoted  :=  (not Special)  (not Space)(*)	
	//  EndSC    :=  Newline  ';'
	//  SCString :=  ';'  (not EndSC)(*)  EndSC
	//  SQString  :=  '\''  (not '\'')(+)  '\''
	//  DQString  :=  '"'  (not '"')(+)  '"'
	//  Value     :=  SQString  |  DQString  |  SCString  |  Unquoted
    function uqAction(c, cs) {
        return c + cs; /* This is just pushing the character onto the front of the string, right? */
    }
    
    var unquoted = PC.app(uqAction, special.not1(), space.not1().many0()),
        endSC = PC.all([newline, PC.literal(';')]), // I guess the return value isn't important here
        scString = PC.literal(';').seq2R(endSC.not1().many0()).seq2L(endSC),
        sq = PC.literal("'"),
        sqString = sq.seq2R(sq.not1().many1()).seq2L(sq),
        dq = PC.literal('"'),
        dqString = dq.seq2R(dq.not1().many1()).seq2L(dq),
        value = PC.any([sqString, dqString, scString, unquoted]);
	
	//  Whitespace  :=  Blank(+)
	//  Newlines    :=  Newline(+)
    var whitespace = blank.many1(),
        newlines = newline.many1();
	
	//  Token   :=  DataOpen  |  SaveOpen  |  SaveClose  |  Loop  |  Stop  |  Value  
	//           |  Whitespace  |  Newlines  |  Comment  |  Identifier
	var token = PC.any([dataOpen, saveOpen, saveClose, loop, stop, value, whitespace, newlines, comment, identifier]);
    
	//  Scanner  :=  Token(*)
    var scanner = token.many0();
    
    function loadFile(i, f, g) {
        $.get('http://rest.bmrb.wisc.edu/bmrb/NMR-STAR3/' + i, // just assume i is an integer 
              f); // awesome, can't use $.ajax
    }
	
    return {
        scanner:  scanner,
        token:  token,
        dataOpen: dataOpen,
        saveOpen: saveOpen,
        saveClose: saveClose,
        loop: loop,
        stop: stop, 
        value: value,
        whitespace: whitespace,
        newlines: newlines,
        comment: comment,
        identifier: identifier,
        newline: newline,
        blank: blank,
        space: space,
        special: special,
        loadFile: loadFile
    };
    
})(ParserCombs, $);