grammar sampleGrm;

start: (importStatement | classStatement)* EOF;

//import
importStatement: (simpleImport | importFrom);
packageName : IDEN ('.' IDEN)*;

simpleImport: IMPORT packageName (',' packageName)* SEMICOL;
importFrom: FROM packageName IMPORT ('*' | packageName (',' packageName)* ('=>' IDEN)? ) SEMICOL;

//----------------------------------------var:

//-------------------varValue
// IDEN?
varValue: ( intVal | floatVal | boolVal | normalFormVal | STRING | oop);
intVal: INT;
floatVal: FLOAT;
boolVal: BOOLEAN;
normalFormVal: FLOAT ('e' | 'E') (| '-' | '+') INT;
oop: IDEN ('.' IDEN)*;

//---------------------varDeclaration
varDefSemi: varDef SEMICOL;
varDef: normalVarDef | classVarDef;

classVarDef: ('private' | 'public' | 'protected')? normalVarDef;
normalVarDef: objectDef | primitiveDef;

objectDef: ('var'|'const') objectName ':' 'new' className '(' (expressionList)? ')' /*SEMICOL*/;
objectName: IDEN;
className: IDEN;

primitiveDef: primConstDef | primVarDef;
primConstDef: 'const' varName (':' varType)? '=' (expression | 'Array' '(' (expressionList)? ')' ) /*SEMICOL*/;
expressionList: expression (',' expression)*;
varName: IDEN;

//var myVar : Int = 25, myStr : String, myStr2 : String = "abcd", myArray1 : new Array [Double] (4.5, 6.77); is OK and gets accepted!
primVarDef: simpleVarDef | complexVarDef;
simpleVarDef: varType (varName ('=' expression)?) (',' varName ('=' expression)?)*/*SEMICOL*/;

complexVarDef: 'var' (complexVarDefTemp (',' complexVarDefTemp)*) /*SEMICOL*/;
complexVarDefTemp: varName ':' ((varType ('=' expression)?) | ('new' 'Array' ('[' varType ']')? '(' (expressionList)? ')') );

//--------varType
varType: 'Int' | 'Double' | 'String' | 'Boolean' | 'Char';


//-----------------------------varAssign
varAssignSemi: varAssign SEMICOL;
varAssign: simpleVarAssign | incOrDec;
simpleVarAssign: varName ('.' varName)* ('='|'+='|'-='|'*='|'/=') expression ('[' INT ']')? /*SEMICOL*/;
incOrDec: varName ('.' varName)* ('++' | '--') /*SEMICOL*/;



//----------classes:
classStatement: CLASS IDEN (EXTENDS IDEN)? (IMPLEMENTS IDEN (WITH IDEN)*)? '{' classBody '}';

classBody: (functions | varDefSemi | varAssignSemi)*;

//----------------func
functions: constructor | normalFunc;

constructor: IDEN '(' funcInputList? ')' '{' funcCode '}';
normalFunc: notVoidFunc | voidFunc;

voidFunc: ('private'|'public'|'protected')? 'void' funcName '(' funcInputList? ')' '{' funcCode '}';
notVoidFunc: ('private'|'public'|'protected')? returnType funcName '(' funcInputList? ')' '{' funcCode  returnStatement'}';
returnStatement: 'return' (varName | expression) SEMICOL;

returnType: (INT_NAME | DOUBLE_NAME | STRING_NAME | BOOLEAN_NAME | CHAR_NAME);
funcName: IDEN;

funcInputList: funcInput (',' funcInput)*;
funcInput: normalVarDef;

funcCode: codeBlock;

codeBlock: ((normalVarDef SEMICOL) | varAssignSemi | ifStatement | forStatement | whileLoop |switchCaseStatement | tryCatchStatement)*;

//condition:
conditionStatement: (expression)+;

//------if
ifStatement: 'if' '(' conditionStatement ')' '{' codeBlock '}' ('elif' '(' conditionStatement ')' '{' codeBlock '}' )* ('else' '{' codeBlock '}' )?;

//-----for
forStatement: forLoop | forIterative;

forLoop: 'for' '(' initList? ';' conditionStatement? ';' forIncOrDecList? ')' '{' codeBlock '}';
initList: (normalVarDef| varAssign) (',' (varAssign | normalVarDef))*;
forIncOrDecList: varAssign (',' varAssign)*;

forIterative: 'for' '(' ('var' | 'const')? varName 'in' iteratorName ')' '{' codeBlock '}';
iteratorName: varName;

//-----while loop
whileLoop: while | doWhile;
while: 'while' '(' conditionStatement ')' '{' codeBlock '}';
doWhile: 'do' '{' codeBlock '}' 'while' '(' conditionStatement ')';

//---switch case
switchCaseStatement: 'switch' '(' expression ')' '{' switchBody '}';
switchBody: (caseBody)+ ('default' ':' codeBlock ('break' SEMICOL)? )?;
caseBody: 'case' varValue ':' codeBlock ('break' SEMICOL)?;

tryCatchStatement: 'try' '{' codeBlock '}' (catchStatement)* lastCatchStatement;
catchStatement: 'on' exceptionName ('catch' '('varName')')? '{' codeBlock '}';
lastCatchStatement: 'catch' '(' varName ')' '{' codeBlock '}';
exceptionName: IDEN;


/*
funcStatement: ACCESSTYPE? VARTYPE? IDEN '(' funcInputStatement ')' '{' funcBody '}';
funcInputStatement: NUMBER '+' NUMBER;
funcBody: (IDEN)*;
*/

//constructor:

//voids:







//experession
expression :
            '(' expression ')'
            | expression '**' <assoc = right> expression
            | '~' expression
            | ('-'| '+') expression
            | expression op=('*' | '/' | '//' | '%') expression
            | expression op=('+' | '-') expression
            | '#' expression
            | expression ('>>' | '<<')
            | expression ('&' | '|' | '^') expression
            | expression op=('==' | '!=' | '<>') expression
            | expression op=('>' | '>=' | '<' | '<=') expression
            | expression op=( '=' | POWEQ | DIVEQ | OUTEQ | MULTEQ | MODEQ | SUBEQ | ADDEQ) expression
            | NOT expression
            | expression AND expression
            | expression OR expression
            | varValue;

WS: [ \t\r\n]+ -> skip;

INT: [0-9]+;
FLOAT: INT '.' INT | INT | '.' INT;
STRING : '"' (~["\r\n] | '""')* '"' ;
BOOLEAN: 'true' | 'false';

//keywords
INT_NAME: 'Int';
DOUBLE_NAME: 'Double';
STRING_NAME: 'String';
VOID_NAME: 'Void';
BOOLEAN_NAME: 'Boolean';
CHAR_NAME: 'Char';

CONST: 'const';
IMPORT: 'import';
FROM: 'from';
CLASS: 'class';
EXTENDS: 'extends';
IMPLEMENTS: 'implements';
WITH: 'with';
ACCESSTYPE: 'protected' | 'private' | 'public';

SEMICOL: ';';
NUMBER: [0-9]+;
fragment LETTER: [a-zA-Z];
fragment CHARACTER: [a-zA-Z0-9];
IDEN: LETTER (CHARACTER|'_'|'$')+;






EQUAL:
            '==';
NOTEQUAL:
            '!=';
ADDEQ:
            '+=';
SUBEQ:
            '-=';
MULTEQ:
            '*=';
POWEQ:
            '**=';
DIVEQ:
            '/=';
MODEQ:
            '%=';
OUTEQ:
            '//=';
NOTEQUALTO:
            '<>';
GREATER:
            '>';
EQGREATER:
            '>=';
SMALLER:
            '<';
EQSMALLER:
            '<=';

            AND:
                        '&&';
            OR:
                        '||';
            NOT:
                        '!';


COMMENT: '/*' .*? '*/' -> skip;
WHOLELINECOMMENT: '#' ~[\r\n]* -> skip;

/*
start: statement*;
statement: ifstatement | operation;
ifstatement: 'if' '('condition')' '{' statement* '}'
('else' '{' statement* '}' )?;
condition: VAR | BOOL;

operation: NUMBER '+' NUMBER;


WS: [ \t\r\n]+ -> skip;

//lexer:
BOOL: 'true' | 'false';
fragment LETTER: [a-zA-Z];
VAR: LETTER+;

NUMBER: [0-9]+;*/
