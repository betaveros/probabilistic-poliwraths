use strict;
use warnings;

die "give 2 args, strategy and fileshift" if (scalar @ARGV) != 2;
die "fileshift should = 300n" if $ARGV[1] % 300 != 0;
system "ghc smart.hs -o smart";
$, = " ";
for my $i (3..32) {
	my $j = $ARGV[1] + $i;
	system "./smart $ARGV[0] $i > s$j.txt";
	print `runhaskell score.hs s$i.txt s$j.txt`;
}

