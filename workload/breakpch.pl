#!/usr/bin/perl

@q = ();
$open = 0;
while (<>) {
 if ($_ =~ /^\s*#\s*(.+)\s*$/) {
  chomp $_;
  if ($open) {
   close F;
  }
  $fn = $1;
  $fn =~ tr/ /_/;
  $fn =~ s/_+/_/g;
  print STDERR "$_ -> $fn\n";
  open (F, "> ". $fn);
  $open = 1;
 } else {
  if ($open) {
   print F $_;
  }
 }
}
if ($open) {
 close F;
}
