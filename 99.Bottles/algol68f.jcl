//A68CB99  JOB 1,'99 BOTTLES A68C Func',CLASS=A,
//             MSGCLASS=A,MSGLEVEL=(1,1)
//A68CCLG EXEC A68CLG
//A68.SYSIN DD *
BEGIN
  PROC howmany = (INT i) STRING: (
    STRING rv;
    IF i = 0 THEN
	  rv := "no more bottles of beer"
	ELIF i = 1 THEN
	  rv := "1 bottle of beer"
	ELSE
	  rv := "lotsa bottles of beer"
	FI;
	rv
  );

  INT i;
  INT j;
  FOR i FROM 99 BY -1 TO 0 DO
    print (howmany(i));
	print (" on the wall, ");
    print (howmany(i));
	print (",");
	new line(stand out);
	
	IF i > 0
	 THEN
	   print ("Take one down, pass it around");
	   j := i - 1
	 ELSE
	   print ("Go to the store, buy some more");
	   j := 99
	 FI;
	new line(stand out);
	
    print (howmany(j));
	print (" on the wall.");
	new line(stand out);
	new line(stand out)
  OD
END
//
