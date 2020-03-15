#!/usr/bin/perl

sub readpi {
 my ($fn) = @_;
 open (F, $fn) || die $!;
 my $on = 0;
 my $pi = '';
 while(<F>) {
  s/^\x0c//;
  chomp;
  next if /^$/; 
  if (/^(0?3\.?14159)/) {
   $on = 1;
   s/^0?3\.?14159/3.14159/;
  } if ($_ !~ /^\d/) {
   $on = 0;
  }
  $pi .= $_ if $on;
 }
 close F;
 return $pi;
}

my $ref = readpi('pi_dec_1m.txt');
printf "length of ref = %d\n", length($ref);
my $test = readpi ($ARGV[0]);
#printf "pi = %s\n", $pi;
printf "length of test = %d\n", length($test);

my $same = 1;
foreach my $i (0..length($test)-1) {
 my $a = substr($ref,$i,1);
 my $b = substr($test,$i,1);
 if ($a ne $b) {
  printf "difference in digit %d: ref '%s' <> test '%s'\n", $i, $a, $b;
  $same = 0;
  last;
 }
}
print "digits are correct\n" if $same;
