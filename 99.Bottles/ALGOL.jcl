//ALGOLB99 JOB (DEW,DEW),'99 BOTTLES ALGOL F',CLASS=A,MSGCLASS=A,
//             REGION=1024K,COND=(0,NE),MSGLEVEL=(1,1)
//IVP1    EXEC ALGOFCLG  ,PARM.GO='TRACE'
//ALGOL.SYSIN DD *
'begin'

'comment'
    99 Bottles of Beer on the Wall
    written in Algol60, for the a60 interpreter
    by Erik Schoenfelder.

    Lower case, whitespace, ASCII, literal strings,
    implementation character set, etc, and other    
    modern conceits are ahistoric and waste cards and tape.

    Tom Jennings 5 Jan 2006
;

'integer' 'procedure' bottles(n);
'value' n;
'integer' n;
'begin'
  'if' n < 1 
    'then' outstring(1, '('no more ')')
    'else' outinteger(1, n);
  'if' n = 1 
    'then' outstring(1, '('bottle')') 
    'else' outstring(1, '('bottles')');
  outstring(1, '(' of beer')');
'end';

'comment' Here is main.  ;

'integer' i;

'COMMENT' SET LINE-LENGTH=120,SET LINES-PER-PAGE=62,OPEN;
SYSACT(1,6,120); SYSACT(1,8,62); SYSACT(1,12,1);

'for' i := 99 'step' -1 'until' 0 'do' 'begin'
  bottles(i); outstring(1, '(' on the wall, ')'); bottles(i); 
  SYSACT(1,14,1); 'COMMENT' NEW LINE;	
	
  'if' i > 0
    'then' 'begin'
      outstring(1, '('take one down and pass it around, ')');
      bottles(i - 1);
    'end'
    'else' 'begin'
      outstring(1, '('go to the store, ')');
      outstring(1, '('buy some more, ')');
      bottles(99); 
    'end';
    outstring(1, '(' on the wall.')');
    SYSACT(1,14,2); 'COMMENT' NEW LINE;
  'end';

'end'
//