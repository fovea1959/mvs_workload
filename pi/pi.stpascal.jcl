//PISTPSCL JOB (001),'Pi spigot ST Pascal',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),TIME=20
//STANFORD EXEC STPASCAL,PARM.COMPILE='COMPILATION OPTION LIST',
//             GOTIME=600,TIME.GO=20
//COMPILE.INPUT DD *
Program Pi_Spigot(OUTPUT);
const
  n   = 9000;
  len = 30000; (* 10*n div 3 *)
 
var
  i, j, k, q, x, nines, predigit: integer;
  a: array[0..len] of integer;

  column: integer;
 
procedure writedigit (v : integer; force: boolean);
begin
 if (v < 0) or (v > 9) then
  begin
   writeln;
   write (' ****** ');
   writeln(v);
   column := 1;
  end
 else
  case v of
   0 : write('0');
   1 : write('1');
   2 : write('2');
   3 : write('3');
   4 : write('4');
   5 : write('5');
   6 : write('6');
   7 : write('7');
   8 : write('8');
   9 : write('9');
  end;

 if (column > 130) or force then
  begin
   writeln ();
   write(' '); (* carriage control *)
   column := 1;
  end
 else
  column := column + 1;
end;
 
begin
 column := 1;
 write(' ');

 for j := 1 to len do a[j] := 2; (* Start with 2s *)
 nines := 0; predigit := 0; (* First predigit is a 0 *)
 for j := 1 to n do
  begin
   q := 0;
   for i := len downto 1 do (* Work backwards *)
    begin
     x:=10*a[i] + q*i;
     a[i] := x mod (2*i-1);
     q:=x div (2*i-1);
    end;
   a[1] := q mod 10; q := q div 10;
   if q = 9
    then nines := nines + 1
    else if q = 10 then
     begin
      writedigit(predigit+1, false);
      for k := 1 to nines do writedigit(0,false);(* zeros *)
      predigit := 0; nines := 0
     end
    else 
     begin
      writedigit(predigit, false); predigit := q;
      if nines <> 0 then
       begin
        for k := 1 to nines do writedigit(9, false);
        nines := 0
       end
     end
  end;
 writedigit(predigit, true);
end.
/*
//
