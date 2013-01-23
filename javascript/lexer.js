var Lexer = (function(PC) {
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
    
    var stop = PC.string("stop_").fmap(function(x) {return Token('stop');}),
        saveClose = PC.string("save_").fmap(function(x) {return Token('saveclose');}),
        loop = PC.string("loop_").fmap(function(x) {return Token('loop');}),
        comment = PC.literal('#').seq2R(newline.not1().many0()).fmap(function(cs) {return Token('comment', cs);}), // TODO do we need to join the cs?
        dataOpen = PC.string("data_").seq2R(space.not1().many1()).fmap(function(x) {return Token('dataopen');}),
        saveOpen = saveClose.seq2R(space.not1().many1()).fmap(function(x) {return Token('saveopen');});
	
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
	
	//  OneToken   :=  DataOpen  |  SaveOpen  |  SaveClose  |  Loop  |  Stop  |  Value  
	//              |  Whitespace  |  Newlines  |  Comment  |  Identifier
	var oneToken = PC.any([dataOpen, saveOpen, saveClose, loop, stop, value, whitespace, newlines, comment, identifier]);
    
	//  Scanner  :=  OneToken(*)
    var scanner = oneToken.many0();
    
    var TOKEN_TYPES = {
        'dataopen': 1,
        'saveopen': 1,
        'saveclose': 1,
        'loop': 1,
        'stop': 1,
        'value': 1,
        'whitespace': 1, // CONFUSING -- actually means 'space or tab, but not newline'
        'newline': 1,
        'comment': 1,
        'identifier': 1
    };
	
    function Token(type, value) {
        if(!(type in TOKEN_TYPES)) {
            throw new Error("invalid token type: " + type);
        }
        return {
            type: 'token',
            tokentype: type,
            value: value
        };
    }
    
    return {
        scanner:  scanner,
        oneToken:  oneToken,
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
        Token: Token
    };
    
})(ParserCombs);