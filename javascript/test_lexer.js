function testLexer(lex, me) {

    module("lexer");
    
    var pure = me.pure;
    
    test("blank, newline, space, special", function() {
        var bl = lex.blank;
        var passes = [
            ['\t', ' ab',     bl, '\t ab'],
            [' ',  'fcgdaeb', bl, ' fcgdaeb'],
            ['\n', '',        lex.newline, '\n'],
            ['\r', 'a',       lex.newline, '\ra'],
            ['\f', 'qqq',     lex.newline, '\fqqq'],
            [' ',  'uhoh',    lex.space, ' uhoh'],
            ['\n', 'blargh',  lex.space, '\nblargh'],
            ['#',  'abc',     lex.special, '#abc'],
            ['"',  'no',      lex.special, '"no'],
            ['\n', 'bah',     lex.special, '\nbah']];
        
        passes.map(function(x) {
            deepEqual(pure({result: x[0], rest: x[1]}), x[2].parse(x[3]));
        });
        
        var fails = [
            [bl, '\n\t'],
            [bl, 'abc'],
            [lex.newline, '\tddd'],
            [lex.newline, 'a to z'],
            [lex.space, '33333l'],
            [lex.space, 'leet'],
            [lex.special, 'qrs'],
            [lex.special, '123']];
            
        fails.map(function(x) {
            deepEqual(me.zero, x[0].parse(x[1]));
        });
    });
    
    test("identifier, comment, newlines, whitespace", function() {
        var id = lex.identifier, cm = lex.comment,
            ns = lex.newlines, ws = lex.whitespace;
        var passes = [
            ['_abc123', ' ab',     id, '_abc123 ab'],
            ['_hi', '\n\r\f123', id, '\n\r\f123'],
            ['#comment', '\nduh', cm, '#comment\nduh'],
            ['# this too', '\r\f', cm, '# this too\r\f']];
        
        passes.map(function(x) {
            deepEqual(pure({result: x[0], rest: x[1]}), x[2].parse(x[3]));
        });
        
        var fails = [
            [bl, '\n\t'],
            [bl, 'abc'],
            [lex.newline, '\tddd'],
            [lex.newline, 'a to z'],
            [lex.space, '33333l'],
            [lex.space, 'leet'],
            [lex.special, 'qrs'],
            [lex.special, '123']];
            
        fails.map(function(x) {
            deepEqual(me.zero, x[0].parse(x[1]));
        });

    });

}