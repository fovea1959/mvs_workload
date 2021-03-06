//STPASCAL JOB (001),'99 BOTTLES STPASCAL',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1)
//STANFORD EXEC STPASCAL,PARM.COMPILE='COMPILATION OPTION LIST'
//COMPILE.INPUT DD *
program BottlesOfBeer (output);
(* this program plays the 99 bottles of beer song *)

const
  BOTTLESSTART = 99;
  BOTTLESEND = 1;

type
  tBottles = BOTTLESEND..BOTTLESSTART;

var
  bottles : tBottles;

begin
  for bottles := BOTTLESSTART downto BOTTLESEND do
  begin
    if bottles > 1 then
    begin
      writeln (bottles,' bottles of beer on the wall, ',bottles,
        ' bottles of beer.');
      write (' Take one down, pass it around, ');
      writeln (bottles - 1, ' bottles of beer on the wall.');
      writeln
    end
    else
    begin
      writeln (' 1 bottle of beer on the wall, one bottle of beer.');
      write (' Take one down, pass it around, ');
      writeln (' no more bottles of beer on the wall');
      writeln;
      writeln (
        ' No more bottles of beer on the wall, no more bottles of beer.');
      write (' Go to the store and buy some more, ');
      writeln ('99 bottles of beer on the wall.')
    end
  end
end.
/*
//
