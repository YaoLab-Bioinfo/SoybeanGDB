#!/usr/bin/perl -w
use strict;
my $in_fl = $ARGV[0];
open (IN, $in_fl) || (die "can't open file:$!\n");
while (<IN>) {
 chomp;
 my @t = split "\t";
 foreach my $t (@t) {
  $t =~ s/:.*//g;
 }
 
 print $t[0], "\t", $t[1], "\t", $t[3], "\t", $t[4], "\t";
 for (my $i=9;$i<=2906;$i++) {
  if ($t[$i] eq "0/0") {
   $t[$i] = $t[3];
  }
  elsif ($t[$i] eq "1/1") {
   $t[$i] = $t[4];
  }
  elsif ($t[$i] eq "0/1") {
   $t[$i] = "H";
  }
  elsif ($t[$i] eq "./.") {
   $t[$i] = "N";
  }
 }
 print join "\t", @t[9..2906];
 print "\n";
}
close IN; 
